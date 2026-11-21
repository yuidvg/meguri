-- | ページの取得。同じホストへは同時に1本・間隔2秒以上、全体の同時数は8。
module Meguri.Fetch.Http
  ( Fetcher
  , newFetcher
  , Page (..)
  , fetchPage
  , fetchReq
  , Req (..)
  , hostOf
  , PageClass (..)
  , classify
  , userAgent
  , version
  , repoUrl
  ) where

import Control.Exception (try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Meguri.Fetch.Cache
import Meguri.Sanitize (publicUrl)
import Network.HTTP.Client
import Data.Default (def)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS qualified as TLS
import Network.HTTP.Types (statusCode)
import Protolude hiding (try)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

version :: Text
version = "0.1.0"

repoUrl :: Text
repoUrl = "https://github.com/yuidvg/meguri"

-- | 正直に名乗る。UAの偽装はしない。
userAgent :: Text
userAgent = "meguri/" <> version <> " (+" <> repoUrl <> ")"

data Fetcher = Fetcher
  { manager :: Manager
  , hosts :: MVar (Map Text (MVar UTCTime))
  , slots :: QSem
  , cacheDir :: FilePath
  , fresh :: Bool
  , pagesDir :: Maybe FilePath
  }

newFetcher :: FilePath -> Bool -> Maybe FilePath -> IO Fetcher
newFetcher cacheDir fresh pagesDir = do
  -- Extended Main Secret に対応していないサーバー（hands.net など）にもつなぐ。
  -- tls の既定は「必須」だが、ブラウザや curl と同じく「あれば使う」にする。証明書の検証は外さない。
  let supported = def {TLS.supportedExtendedMainSecret = TLS.AllowEMS}
  manager <- newManager (mkManagerSettings (TLSSettingsSimple False False False supported) Nothing)
  hosts <- newMVar Map.empty
  slots <- newQSem 8
  pure Fetcher {..}

data Page = Page
  { url :: Text
  , status :: Int
  , contentType :: Maybe Text
  , body :: ByteString
  , cached :: Bool
  , fetchedAt :: UTCTime
  }

hostOf :: Text -> Text
hostOf u = case T.splitOn "/" u of
  (_ : "" : h : _) -> h
  _ -> u

minInterval, cacheTtl :: Num a => a
minInterval = 2
cacheTtl = 15 * 60

-- | 取りに行くもの
data Req = Req
  { url :: Text
  , -- | 読み取り専用の POST（カタログに載せたものだけ）。フォームの中身
    form :: Maybe [(Text, Text)]
  , -- | 配送先などを含む。キャッシュにも、実行のページにも残さない
    private :: Bool
  }

fetchPage :: Fetcher -> Text -> IO (Either Text Page)
fetchPage f u = fetchReq f (Req u Nothing False)

fetchReq :: Fetcher -> Req -> IO (Either Text Page)
fetchReq f r
  | not (publicUrl r.url) = pure (Left "公開のホストのURLではないので、取りに行かない")
  | otherwise = fetchPublic f r

fetchPublic :: Fetcher -> Req -> IO (Either Text Page)
fetchPublic f req = do
  let u = req.url
      -- キャッシュのキー：URL と、POST ならフォームの中身
      key = u <> maybe "" (\kvs -> "\n" <> T.intercalate "&" [k <> "=" <> v | (k, v) <- kvs]) req.form
  now <- getCurrentTime
  hit <- if f.fresh || req.private then pure Nothing else readCache f.cacheDir cacheTtl now key
  case hit of
    Just (e, body) -> pure (Right (Page u e.status e.contentType body True e.at))
    Nothing -> do
      lock <- modifyMVar f.hosts $ \m -> case Map.lookup (hostOf u) m of
        Just l -> pure (m, l)
        Nothing -> do
          l <- newMVar (addUTCTime (-minInterval) now)
          pure (Map.insert (hostOf u) l m, l)
      -- ホストごとに1本ずつ。全体の枠は、順番が来てから取る
      r <- modifyMVar lock $ \lastAt -> do
        r <- bracket_ (waitQSem f.slots) (signalQSem f.slots) (paced lastAt (request f req))
        done <- getCurrentTime
        pure (done, r)
      unless req.private $ for_ r $ \p -> do
        when (p.status `elem` [200, 404]) $ writeCache f.cacheDir (CacheEntry key p.status p.contentType p.fetchedAt) p.body
        for_ f.pagesDir $ \d -> do
          createDirectoryIfMissing True d
          BS.writeFile (d </> cacheKey key <> ".html") p.body
      pure r

-- | 前の取得から2秒空ける。503 は一度だけ5秒待って取り直す（楽天は続けて検索すると503が出る）。
paced :: UTCTime -> IO (Either Text Page) -> IO (Either Text Page)
paced lastAt act = do
  now <- getCurrentTime
  let wait = minInterval - realToFrac (diffUTCTime now lastAt) :: Double
  when (wait > 0) $ threadDelay (ceiling (wait * 1000000))
  r <- act
  case r of
    Right p | p.status == 503 -> threadDelay 5000000 >> act
    _ -> pure r

request :: Fetcher -> Req -> IO (Either Text Page)
request f r = case parseRequest (toS r.url) of
  Nothing -> pure (Left "URLの形が正しくない")
  Just req0 -> do
    let u = r.url
        withForm = case r.form of
          Just kvs -> urlEncodedBody [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- kvs]
          Nothing -> identity
        req =
          withForm
            req0
            { requestHeaders =
                [ ("User-Agent", encodeUtf8 userAgent)
                , ("Accept", "text/html,application/xhtml+xml,application/json;q=0.9,*/*;q=0.8")
                , ("Accept-Language", "ja,en;q=0.5")
                ]
            , responseTimeout = responseTimeoutMicro 20000000
            }
    r <- try (httpLbs req f.manager)
    now <- getCurrentTime
    pure $ case r of
      Left (e :: HttpException) -> Left (describe e)
      Right resp ->
        Right
          Page
            { url = u
            , status = statusCode (responseStatus resp)
            , contentType = TE.decodeLatin1 <$> lookup "Content-Type" (responseHeaders resp)
            , body = BL.toStrict (responseBody resp)
            , cached = False
            , fetchedAt = now
            }
 where
  describe = \case
    HttpExceptionRequest _ c -> case c of
      ResponseTimeout -> "応答なし（20秒）"
      ConnectionTimeout -> "接続できない（時間切れ）"
      ConnectionFailure _ -> "接続できない"
      TooManyRedirects _ -> "転送が多すぎる"
      InternalException e -> "TLS などの接続の失敗：" <> T.take 80 (show e)
      other -> T.take 80 (show other)
    InvalidUrlException _ why -> "URLの形が正しくない：" <> toS why

data PageClass
  = PageOk
  | PageNotFound
  | PageBlocked Text
  | PageStopped Text
  | PageError Text
  deriving stock (Eq, Show)

-- | 応答を分ける（純粋）。403・429・ボット確認・Access Denied はブロック、メンテナンスは停止。
classify :: Int -> Text -> PageClass
classify status bodyText
  | status `elem` [403, 429] = PageBlocked ("HTTP " <> show status)
  | any (`T.isInfixOf` top) botMarks = PageBlocked "ボット確認・Access Denied"
  | maintenance && (status >= 500 || T.length bodyText < 20000) = PageStopped "メンテナンスのページ"
  | status == 404 = PageNotFound
  | status >= 200 && status < 300 = PageOk
  | otherwise = PageError ("HTTP " <> show status)
 where
  top = T.take 6000 bodyText
  botMarks =
    [ "Access Denied"
    , "Pardon Our Interruption"
    , "Just a moment..."
    , "/sorry/index"
    , "g-recaptcha"
    , "captcha-delivery"
    , "Anti-bot check"
    , "ロボットではありません"
    ]
  maintenance =
    any
      (`T.isInfixOf` top)
      ["メンテナンス中", "ただいまメンテナンス", "システムメンテナンスのため", "メンテナンスのお知らせ", "サービスを一時停止", "一時停止のお知らせ", "サービス停止のお知らせ", "一時休止して", "休止のお知らせ"]
