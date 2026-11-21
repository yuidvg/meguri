-- | 実行の記録。search が1回走るたびに $XDG_CACHE_HOME/meguri/runs/<id>/ に書き、7日で消す。
-- 書くだけで、あとのコマンドが読んで続きに使うことはない。
module Meguri.Run
  ( Paths (..)
  , getPaths
  , newRun
  , purgeOld
  , writeLines
  , line
  ) where

import Data.Aeson (ToJSON, Value (..), encode, toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Protolude
import System.Directory
import System.FilePath ((</>))

data Paths = Paths
  { httpCache :: FilePath
  , -- | 判定（jev）の答えのキャッシュ
    judgeCache :: FilePath
  , runsDir :: FilePath
  }

getPaths :: IO Paths
getPaths = do
  base <- getXdgDirectory XdgCache "meguri"
  pure Paths {httpCache = base </> "http", judgeCache = base </> "judge", runsDir = base </> "runs"}

-- | 実行の記録を置くフォルダを作る
newRun :: Paths -> IO (Text, FilePath)
newRun p = do
  now <- getCurrentTime
  let rid = toS (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" (addUTCTime (9 * 3600) now))
      dir = p.runsDir </> rid
  createDirectoryIfMissing True dir
  pure (toS rid, dir)

-- | 7日より古い実行と、1日より古い HTTP のキャッシュを消す
purgeOld :: Paths -> IO ()
purgeOld p = do
  now <- getCurrentTime
  sweep now (7 * 86400) p.runsDir True
  sweep now 86400 p.httpCache False
  sweep now 86400 p.judgeCache False
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

writeLines :: FilePath -> [ByteString] -> IO ()
writeLines path ls = unless (null ls) $ BS.writeFile path (BC.unlines ls)
