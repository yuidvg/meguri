-- | 文字コード。HTTPヘッダ → meta → 中身の順に決める。Shift_JIS は cp932 として読む。
module Meguri.Fetch.Encoding
  ( Charset (..)
  , decodeBody
  , decideCharset
  , charsetFromName
  , decodeAs
  , encodeQuery
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.ICU.Convert qualified as ICU
import Meguri.Catalog (Enc (..))
import Protolude
import System.IO.Unsafe (unsafePerformIO)

data Charset = CsUtf8 | CsCp932 | CsEucJp
  deriving stock (Eq, Show)

charsetFromName :: Text -> Maybe Charset
charsetFromName n = case T.toLower (T.filter (\c -> isAlphaNum c) n) of
  "utf8" -> Just CsUtf8
  x | x `elem` ["shiftjis", "sjis", "xsjis", "windows31j", "ms932", "cp932", "mskanji", "csshiftjis"] -> Just CsCp932
  x | x `elem` ["eucjp", "xeucjp", "cseucpkdfmtjapanese"] -> Just CsEucJp
  _ -> Nothing

-- | @charset=...@ の値を拾う
charsetParam :: ByteString -> Maybe Charset
charsetParam bs =
  let lower = BC.map toLowerAscii bs
      (_, hit) = BS.breakSubstring "charset" lower
      rest = BC.dropWhile (`elem` ("= \"'" :: [Char])) (BS.drop 7 hit)
      name = BC.takeWhile (\c -> isAlphaNum c || c `elem` ("-_" :: [Char])) rest
   in if BS.null hit then Nothing else charsetFromName (TE.decodeLatin1 name)
 where
  toLowerAscii c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | HTTPヘッダ → meta（先頭 4KB）→ 中身（UTF-8 として読めるか）の順
decideCharset :: Maybe Text -> ByteString -> Charset
decideCharset contentType body =
  fromMaybe byContent ((charsetParam . encodeUtf8 =<< contentType) <|> charsetParam (BS.take 4096 body))
 where
  byContent = either (const CsCp932) (const CsUtf8) (decodeUtf8' body)

decodeBody :: Maybe Text -> ByteString -> (Charset, Text)
decodeBody contentType body = let cs = decideCharset contentType body in (cs, decodeAs cs body)

decodeAs :: Charset -> ByteString -> Text
decodeAs = \case
  CsUtf8 -> TE.decodeUtf8With lenientDecode
  CsCp932 -> icuDecode "windows-31j"
  CsEucJp -> icuDecode "EUC-JP"

-- ICU の変換器は使い回せない（状態を持つ）ので、呼び出しごとに開く
icuDecode :: [Char] -> ByteString -> Text
icuDecode name bs = unsafePerformIO $ do
  c <- ICU.open name Nothing
  pure $! ICU.toUnicode c bs
{-# NOINLINE icuDecode #-}

icuEncode :: [Char] -> Text -> ByteString
icuEncode name t = unsafePerformIO $ do
  c <- ICU.open name Nothing
  pure $! ICU.fromUnicode c t
{-# NOINLINE icuEncode #-}

-- | 検索語を、サイトの文字コードのバイト列にする
encodeQuery :: Enc -> Text -> ByteString
encodeQuery = \case
  Utf8 -> encodeUtf8
  ShiftJis -> icuEncode "windows-31j"
  Cp932 -> icuEncode "windows-31j"
  EucJp -> icuEncode "EUC-JP"
