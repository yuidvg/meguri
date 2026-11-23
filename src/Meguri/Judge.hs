-- | 判定を外に任せる：外のコマンドか、判定モデル jev（OpenRouter の Decisions API）。
--
-- meguri 自身は「同じ品か」「付属品か」を判断しない。頼まれた判定を、頼まれた相手に渡して、答えを受け取るだけ。
--
-- * 外のコマンド：行を JSON Lines で標準入力に渡し、残すものの id を標準出力で受け取る。
-- * jev：1行につき1回、その行を「状態」として、はい／いいえの質問をする。「はい」の確率が 0.5 以上なら残す。
--   @OPENROUTER_API_KEY@ が要る。キーはログに入れない。答えはキャッシュしない。
module Meguri.Judge
  ( JevClient (..)
  , newJev
  , asker
  , commandInput
  , commandKept
  , runCommand
  , jevRequest
  , jevAnswer
  , jevKeeps
  , defaultModel
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try)
import Data.Aeson (Value (..), decodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Set qualified as Set
import Data.Text qualified as T
import Meguri.Selection (Ask, JudgeBy (..))
import Network.HTTP.Client qualified as H
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Protolude hiding (try)
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), shell, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

-- | jev を呼ぶための道具
data JevClient = JevClient
  { key :: Text
  , model :: Text
  , manager :: H.Manager
  , slots :: QSem
  }

-- | 既定のモデル。確率のしきい値はモデルごとに意味が変わるので、版を固定する
defaultModel :: Text
defaultModel = "typesafe/jev-1.14"

-- | キーとモデルの名前から作る。同時に呼ぶのは8本まで
newJev :: Text -> Maybe Text -> IO JevClient
newJev key model = do
  manager <- newTlsManager
  slots <- newQSem 8
  pure JevClient {key, model = fromMaybe defaultModel model, manager, slots}

-- | 判定の頼み先を決める。jev は、キーがあるときだけ使える
asker :: Maybe JevClient -> Ask
asker jev by rows = case (by, jev) of
  (Command cmd, _) -> runCommand cmd rows
  (Jev _, Nothing) -> pure (Left "OPENROUTER_API_KEY が無い（jev は OpenRouter 経由で呼ぶ。キーを環境変数に入れる）")
  (Jev question, Just j) -> askJev j question rows

------------------------------------------------------------------------------
-- 外のコマンド

-- | 外のコマンドの標準入力：1行に1つの JSON
commandInput :: [Value] -> ByteString
commandInput = BC.unlines . map (BL.toStrict . encode)

-- | 外のコマンドの標準出力から、残すものの id を読む。
--
-- 1行に1つ、残すものを書く。id だけ（@12@・@rakuten@）でも、id を持つ JSON（入力の行そのまま、でもよい）でもよい。
-- id は、数でも文字列でもよい（売り物は番号、販路はその id）。
-- JSON に @"keep": false@ があれば、それは外す。出てこなかった id は外す。
-- つまり @jq -c 'select(...)'@ のように「残す行をそのまま出す」コマンドが、そのまま判定になる。
commandKept :: ByteString -> Either Text (Set Text)
commandKept out =
  let ls = filter (not . BS.null) (map BC.strip (BC.lines out))
      parsed = map verdict ls
   in if not (null ls) && all isNothing parsed
        then Left "出力を読めなかった（残すものの id か、id を持つ JSON を、1行に1つ出す）"
        else Right (Set.fromList [i | Just (Just i) <- parsed])
 where
  -- 読めない行は Nothing。読めて「外す」なら Just Nothing
  verdict :: ByteString -> Maybe (Maybe Text)
  verdict l = case decodeStrict' l of
    Just (Object o)
      | KM.lookup "keep" o == Just (Bool False) -> Nothing <$ (idOf =<< KM.lookup "id" o)
      | otherwise -> Just <$> (idOf =<< KM.lookup "id" o)
    Just v -> Just <$> idOf v
    -- JSON でない行は、空白を含まない1語なら id として読む（id をそのまま出すコマンド用）
    Nothing -> case decodeUtf8' l of
      Right t | not (T.null t) && T.all (\c -> isAlphaNum c && isAscii c || c == '-' || c == '_') t -> Just (Just t)
      _ -> Nothing

idOf :: Value -> Maybe Text
idOf = \case
  Number n -> show <$> (toBoundedInteger n :: Maybe Int)
  String t | not (T.null t) -> Just t
  _ -> Nothing

-- | 外のコマンドを走らせる（シェル経由）。行の中身は標準入力で渡し、コマンドの文字列には混ぜない。
-- 5分で打ち切る。終了コードが 0 でなければ失敗。
runCommand :: Text -> [Value] -> IO (Either Text (Set Text))
runCommand cmd rows = do
  r <- try (timeout (300 * 1000000) (withCreateProcess spec talk))
  pure $ case r of
    Left (e :: SomeException) -> Left ("走らせられなかった：" <> T.take 120 (show e))
    Right Nothing -> Left "5分たっても終わらなかった"
    Right (Just (ExitFailure n, _)) -> Left ("終了コード " <> show n)
    Right (Just (ExitSuccess, out)) -> commandKept out
 where
  spec = (shell (toS cmd)) {std_in = CreatePipe, std_out = CreatePipe}
  talk (Just hin) (Just hout) _ ph = do
    hSetBinaryMode hin True
    hSetBinaryMode hout True
    -- 入力を最後まで読まないコマンド（head など）でも止まらないように、書き込みの失敗は無視する
    let feed = void (try (BS.hPut hin (commandInput rows) `finally` hClose hin) :: IO (Either IOException ()))
    (_, out) <- concurrently feed (BS.hGetContents hout)
    code <- waitForProcess ph
    pure (code, out)
  talk _ _ _ ph = do
    code <- waitForProcess ph
    pure (code, "")

------------------------------------------------------------------------------
-- jev

-- | jev への頼みの中身（純粋）。行から id を除いたものが「状態」。質問は1つ（はい／いいえ）
jevRequest :: Text -> Text -> Value -> Value
jevRequest model question row =
  object
    [ "model" .= model
    , "state" .= withoutId row
    , "questions" .= object ["keep" .= object ["type" .= ("noul" :: Text), "instructions" .= question]]
    ]
 where
  withoutId = \case
    Object o -> Object (KM.delete "id" o)
    v -> v

-- | jev の答えから、「はい」の確率を読む（純粋）。断られたら、そう返す
jevAnswer :: ByteString -> Either Text Double
jevAnswer body = case decodeStrict' body of
  Just (Object o) -> case KM.lookup "answers" o >>= field "keep" of
    Just (Object a) -> case (KM.lookup "type" a, KM.lookup "noul" a) of
      (_, Just (Number p)) -> Right (toRealFloat p)
      (Just (String "refusal"), _) -> Left "jev が答えを断った"
      _ -> Left "jev の答えに確率が無い"
    _ -> Left ("jev の答えを読めなかった" <> maybe "" ("：" <>) (errorMessage o))
  _ -> Left "jev の答えが JSON でない"
 where
  field k = \case
    Object x -> KM.lookup k x
    _ -> Nothing
  errorMessage o = case KM.lookup "error" o >>= field "message" of
    Just (String m) -> Just (T.take 160 m)
    _ -> Nothing

-- | 「はい」の確率が 0.5 以上なら残す
jevKeeps :: Double -> Bool
jevKeeps = (>= 0.5)

-- | 行ごとに jev に聞く。1つでも失敗したら、全体を失敗にする（聞けなかった行を、残したことにも外したことにもしない）
askJev :: JevClient -> Text -> [Value] -> IO (Either Text (Set Text))
askJev j question rows = do
  answers <- mapConcurrently one rows
  pure $ case lefts answers of
    e : _ -> Left e
    [] -> Right (Set.fromList (catMaybes (rights answers)))
 where
  one row = case rowId row of
    Nothing -> pure (Left "行に id が無い")
    Just i -> fmap (\p -> if jevKeeps p then Just i else Nothing) <$> probability (jevRequest j.model question row)
  rowId = \case
    Object o -> idOf =<< KM.lookup "id" o
    _ -> Nothing
  probability req = bracket_ (waitQSem j.slots) (signalQSem j.slots) (post (BL.toStrict (encode req)))
  post payload = do
    r <- try $ do
      base <- H.parseRequest "https://openrouter.ai/api/alpha/decisions"
      let req =
            base
              { H.method = "POST"
              , H.requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 j.key), ("Content-Type", "application/json")]
              , H.requestBody = H.RequestBodyBS payload
              , H.responseTimeout = H.responseTimeoutMicro (30 * 1000000)
              }
      H.httpLbs req j.manager
    pure $ case r of
      -- 例外の文面には頼みの中身（ヘッダーを含む）が入りうるので、出さない
      Left (_ :: H.HttpException) -> Left "jev に届かなかった（通信の失敗）"
      Right res ->
        let code = statusCode (H.responseStatus res)
            body = BL.toStrict (H.responseBody res)
         in if code == 200
              then jevAnswer body
              else Left ("jev が HTTP " <> show code <> " を返した" <> hint code <> either ("。" <>) (const "") (jevAnswer body))
  hint :: Int -> Text
  hint = \case
    401 -> "（OPENROUTER_API_KEY が正しくない）"
    402 -> "（OpenRouter の残高が足りない）"
    429 -> "（頼みすぎ。少し待って、もう一度走らせる）"
    _ -> ""
