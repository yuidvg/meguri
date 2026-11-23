-- | 文字列の正規化（純粋）
module Meguri.Text
  ( toHalfWidth
  , contains
  , clean
  , decodeEntities
  , squish
  , percentEncode
  , percentDecode
  , queryParam
  , digitsOnly
  , normalizeModel
  , jstStamp
  , shortStamp
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Numeric (showHex)
import Protolude

-- | 語を含むか（大文字小文字・全角半角・空白・ハイフンの違いは無視）。@contains 語 文字列@
contains :: Text -> Text -> Bool
contains w n = fold w `T.isInfixOf` fold n
 where
  fold = T.toLower . T.filter (\c -> not (isSpace c) && c /= '-') . toHalfWidth

-- | 全角の英数字・記号を半角にする（「１，９８０円」→「1,980円」）
toHalfWidth :: Text -> Text
toHalfWidth = T.map f
 where
  f c
    | c >= '\xFF01' && c <= '\xFF5E' = chr (ord c - 0xFEE0)
    | c == '\x3000' = ' '
    | c == '\xFFE5' = '¥'
    | c == '\x2212' = '-'
    | otherwise = c

-- | よく出る HTML の実体参照を文字に戻す（JSON-LD の文字列に入っていることがある）
decodeEntities :: Text -> Text
decodeEntities t
  | T.any (== '&') t =
      foldl' (\acc (from, to) -> T.replace from to acc) t $
        [("&lt;", "<"), ("&gt;", ">"), ("&quot;", "\""), ("&#39;", "'"), ("&#039;", "'"), ("&apos;", "'"), ("&nbsp;", " "), ("&yen;", "¥"), ("&amp;", "&")]
  | otherwise = t

-- | 空白をまとめ、前後を落とす
squish :: Text -> Text
squish = T.unwords . T.words . T.map (\c -> if isSpace c then ' ' else c)

-- | 出力用：制御文字を除き、空白をまとめ、長さを切る
clean :: Int -> Text -> Text
clean n t =
  let s = squish (T.filter (\c -> not (isControl c) || isSpace c) t)
   in if T.length s > n then T.take (n - 1) s <> "…" else s

-- | バイト列を %XX にする（英数字と -._~ はそのまま）
percentEncode :: ByteString -> Text
percentEncode = T.concat . map enc . BS.unpack
 where
  enc w
    | isUnreserved (chr (fromIntegral w)) = T.singleton (chr (fromIntegral w))
    | otherwise = "%" <> T.toUpper (T.justifyRight 2 '0' (toS (showHex w "")))
  isUnreserved c = isAscii c && (isAlphaNum c || c `elem` ("-._~" :: [Char]))

-- | %XX を戻す（UTF-8 として読めなければ 'Nothing'）
percentDecode :: Text -> Maybe Text
percentDecode t = either (const Nothing) Just (decodeUtf8' (BS.pack (go (T.unpack t))))
 where
  go ('%' : a : b : rest) | Just n <- readMaybe ['0', 'x', a, b] = n : go rest
  go ('+' : rest) = 0x20 : go rest
  go (c : rest) = BS.unpack (encodeUtf8 (T.singleton c)) <> go rest
  go [] = []

-- | URL のクエリから、名前の値を取り出して戻す
queryParam :: Text -> Text -> Maybe Text
queryParam key url =
  let query = T.takeWhile (/= '#') (T.drop 1 (T.dropWhile (/= '?') url))
   in percentDecode =<< head [T.drop (T.length key + 1) kv | kv <- T.splitOn "&" (T.replace "&amp;" "&" query), (key <> "=") `T.isPrefixOf` kv]

digitsOnly :: Text -> Text
digitsOnly = T.filter isDigit

-- | 型番の比較用：半角・大文字にして、英数字だけ残す
normalizeModel :: Text -> Text
normalizeModel = T.toUpper . T.filter (\c -> isAscii c && isAlphaNum c) . toHalfWidth

-- | JST の時刻（ISO 8601）
jstStamp :: UTCTime -> Text
jstStamp t = toS (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+09:00" (addUTCTime (9 * 3600) t))

-- | 表示用の短い時刻（「11/20 16:50」）
shortStamp :: Text -> Text
shortStamp s = case T.splitOn "T" s of
  [d, t] | [_, m, dd] <- T.splitOn "-" d -> m <> "/" <> dd <> " " <> T.take 5 t
  _ -> s
