-- | ページの取得。応答はキャッシュしない。
--
-- 同じホストへは、ほかの meguri のプロセスとあわせて同時に1本、間隔は2秒以上（'takeTurn'）。
-- 全体の同時数は、プロセスごとに8。
module Meguri.Fetch.Http
  ( Fetcher
  , newFetcher
  , takeTurn
  , Fetched (..)
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
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime)
import Meguri.Sanitize (publicUrl)
import Network.HTTP.Client
import Data.Default (def)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS qualified as TLS
import Network.HTTP.Types (statusCode)
import Protolude hiding (try)
import System.Directory (createDirectoryIfMissing)
import System.FileLock (SharedExclusive (..), lockFile, unlockFile)
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
  , -- | プロセスの中で、ホストごとに順番を待つ
    hosts :: MVar (Map Text (MVar ()))
  , slots :: QSem
  , -- | ホストごとのロックファイルを置く場所。ほかの meguri のプロセスと分け合う
    locksDir :: FilePath
  , -- | 取ったページを置く場所（実行の記録）
    pagesDir :: Maybe FilePath
  }

newFetcher :: FilePath -> Maybe FilePath -> IO Fetcher
newFetcher locksDir pagesDir = do
  -- Extended Main Secret に対応していないサーバー（hands.net など）にもつなぐ。
  -- tls の既定は「必須」だが、ブラウザや curl と同じく「あれば使う」にする。証明書の検証は外さない。
  let supported = def {TLS.supportedExtendedMainSecret = TLS.AllowEMS}
  manager <- newManager (mkManagerSettings (TLSSettingsSimple False False False supported) Nothing)
  hosts <- newMVar Map.empty
  slots <- newQSem 8
  pure Fetcher {..}

data Fetched = Fetched
  { url :: Text
  , status :: Int
  , contentType :: Maybe Text
  , body :: ByteString
  , fetchedAt :: UTCTime
  }

hostOf :: Text -> Text
hostOf u = case T.splitOn "/" u of
  (_ : "" : h : _) -> h
  _ -> u

-- | 同じホストへの取得の間隔（秒）
minInterval :: Int
minInterval = 2

-- | 取りに行くもの
data Req = Req
  { url :: Text
  , -- | 読み取り専用の POST（カタログに載せたものだけ）。フォームの中身
    form :: Maybe [(Text, Text)]
  , -- | 配送先などを含む。実行の記録のページに残さない
    private :: Bool
  }

fetchPage :: Fetcher -> Text -> IO (Either Text Fetched)
fetchPage f u = fetchReq f (Req u Nothing False)

fetchReq :: Fetcher -> Req -> IO (Either Text Fetched)
fetchReq f r
  | not (publicUrl r.url) = pure (Left "公開のホストのURLではないので、取りに行かない")
  | otherwise = fetchPublic f r

fetchPublic :: Fetcher -> Req -> IO (Either Text Fetched)
fetchPublic f req = do
  -- 全体の枠は、ホストの順番が来てから取る
  r <- takeTurn f (hostOf req.url) (once503 (bracket_ (waitQSem f.slots) (signalQSem f.slots) (request f req)))
  unless req.private $ for_ r $ \p -> for_ f.pagesDir $ \d -> do
    createDirectoryIfMissing True d
    BS.writeFile (d </> pageName req <> ".html") p.body
  pure r

-- | 同じホストへは、ほかの meguri のプロセスとあわせて同時に1本、前の取得が終わってから2秒以上空ける。
--
-- プロセスの中ではホストごとの 'MVar' で、プロセスの間ではホストごとのロックファイル（flock）で順番を待つ。
-- 取り終わってから2秒たつまでロックを放さないので、前の取得の時刻をどこにも書かずに済む。
-- プロセスが途中で終わっても、ロックは OS が外す。
takeTurn :: Fetcher -> Text -> IO a -> IO a
takeTurn f host act = do
  local <- modifyMVar f.hosts $ \m -> case Map.lookup host m of
    Just l -> pure (m, l)
    Nothing -> do
      l <- newMVar ()
      pure (Map.insert host l m, l)
  withMVar local $ \() -> do
    createDirectoryIfMissing True f.locksDir
    bracket
      (lockFile (f.locksDir </> lockName host) Exclusive)
      (\l -> threadDelay (minInterval * 1000000) `finally` unlockFile l)
      (const act)

-- | ロックファイルの名前。ホスト名の英数字・点・ハイフンだけを使う
lockName :: Text -> FilePath
lockName = toS . T.map (\c -> if isAscii c && (isAlphaNum c || c == '.' || c == '-') then c else '_') . T.toLower

-- | 503 は一度だけ、5秒待って取り直す（楽天は、続けて検索すると503が出る）
once503 :: IO (Either Text Fetched) -> IO (Either Text Fetched)
once503 act = do
  r <- act
  case r of
    Right p | p.status == 503 -> threadDelay 5000000 >> act
    _ -> pure r

-- | 実行の記録に置くページの名前：URL（POST ならフォームの中身も）の SHA-256
pageName :: Req -> FilePath
pageName r = toS . TE.decodeLatin1 . B16.encode . SHA256.hash . encodeUtf8 $ r.url <> maybe "" (\kvs -> "\n" <> T.intercalate "&" [k <> "=" <> v | (k, v) <- kvs]) r.form

request :: Fetcher -> Req -> IO (Either Text Fetched)
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
          Fetched
            { url = u
            , status = statusCode (responseStatus resp)
            , contentType = TE.decodeLatin1 <$> lookup "Content-Type" (responseHeaders resp)
            , body = BL.toStrict (responseBody resp)
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
