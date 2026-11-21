-- | 応答のキャッシュ（検索と個別ページは15分）
module Meguri.Fetch.Cache
  ( CacheEntry (..)
  , cacheKey
  , readCache
  , writeCache
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON, decodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Protolude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

data CacheEntry = CacheEntry
  { url :: Text
  , status :: Int
  , contentType :: Maybe Text
  , at :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | キャッシュのキー。URLのハッシュ（URLにAPIキーや配送先を入れない前提）
cacheKey :: Text -> FilePath
cacheKey = toS . TE.decodeLatin1 . B16.encode . SHA256.hash . encodeUtf8

readCache :: FilePath -> NominalDiffTime -> UTCTime -> Text -> IO (Maybe (CacheEntry, ByteString))
readCache dir ttl now u = do
  let base = dir </> cacheKey u
  ok <- (&&) <$> doesFileExist (base <> ".meta") <*> doesFileExist (base <> ".body")
  if not ok
    then pure Nothing
    else do
      meta <- decodeStrict' <$> BS.readFile (base <> ".meta")
      case meta of
        Just e | diffUTCTime now e.at < ttl && diffUTCTime now e.at >= 0 -> Just . (e,) <$> BS.readFile (base <> ".body")
        _ -> pure Nothing

writeCache :: FilePath -> CacheEntry -> ByteString -> IO ()
writeCache dir e body = do
  createDirectoryIfMissing True dir
  let base = dir </> cacheKey e.url
  BS.writeFile (base <> ".body") body
  BL.writeFile (base <> ".meta") (encode e)
