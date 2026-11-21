-- | 実行の状態。$XDG_CACHE_HOME/meguri/runs/<id>/ に置き、7日で消す。
module Meguri.Run
  ( Paths (..)
  , getPaths
  , newRun
  , resolveRun
  , purgeOld
  , appendLines
  , readLines
  , line
  ) where

import Data.Aeson (FromJSON, ToJSON, Value (..), decodeStrict', encode, toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Protolude
import System.Directory
import System.FilePath ((</>))

data Paths = Paths
  { httpCache :: FilePath
  , runsDir :: FilePath
  }

getPaths :: IO Paths
getPaths = do
  base <- getXdgDirectory XdgCache "meguri"
  pure Paths {httpCache = base </> "http", runsDir = base </> "runs"}

-- | 新しい実行を作り、「直前の実行」として覚える
newRun :: Paths -> IO (Text, FilePath)
newRun p = do
  now <- getCurrentTime
  let rid = toS (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" (addUTCTime (9 * 3600) now))
      dir = p.runsDir </> rid
  createDirectoryIfMissing True dir
  TIO.writeFile (p.runsDir </> "latest") (toS rid)
  pure (toS rid, dir)

-- | --run の指定か、直前の実行。無ければ 'Nothing'。
resolveRun :: Paths -> Maybe Text -> IO (Maybe (Text, FilePath))
resolveRun p wanted = do
  rid <- case wanted of
    Just r -> pure (Just r)
    Nothing -> do
      ok <- doesFileExist (p.runsDir </> "latest")
      if ok then Just . T.strip <$> TIO.readFile (p.runsDir </> "latest") else pure Nothing
  case rid of
    Just r | not (T.null r) && T.all (`elem` ("0123456789-" :: [Char])) r -> do
      ok <- doesDirectoryExist (p.runsDir </> toS r)
      pure (if ok then Just (r, p.runsDir </> toS r) else Nothing)
    _ -> pure Nothing

-- | 7日より古い実行と、1日より古い HTTP のキャッシュを消す
purgeOld :: Paths -> IO ()
purgeOld p = do
  now <- getCurrentTime
  sweep now (7 * 86400) p.runsDir True
  sweep now 86400 p.httpCache False
 where
  sweep :: UTCTime -> Double -> FilePath -> Bool -> IO ()
  sweep now age dir dirsOnly = do
    ok <- doesDirectoryExist dir
    when ok $ do
      names <- listDirectory dir
      for_ names $ \n -> do
        let path = dir </> n
        isDir <- doesDirectoryExist path
        when (isDir == dirsOnly) $ do
          t <- getModificationTime path
          when (realToFrac (diffUTCTime now t) > age) $
            if isDir then removePathForcibly path else removeFile path

-- | JSON Lines の1行。版（schema）を付ける。
line :: ToJSON a => Text -> a -> ByteString
line schema a = BL.toStrict . encode $ case toJSON a of
  Object o -> Object (KM.insert "schema" (String schema) o)
  v -> v

appendLines :: FilePath -> [ByteString] -> IO ()
appendLines path ls = unless (null ls) $ BS.appendFile path (BC.unlines ls)

readLines :: FromJSON a => FilePath -> IO [a]
readLines path = do
  ok <- doesFileExist path
  if ok then mapMaybe decodeStrict' . BC.lines <$> BS.readFile path else pure []
