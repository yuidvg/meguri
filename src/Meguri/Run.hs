-- | 実行の記録。search が1回走るたびに $XDG_CACHE_HOME/meguri/runs/<id>/ に書き、7日で消す。
-- 書くだけで、あとのコマンドが読んで続きに使うことはない。
--
-- 同じ時に、ほかの meguri が走っていてもよい。記録のフォルダは実行ごとに別の名前で作り、掃除はぶつかっても止まらない。
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
import Data.Time (addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Protolude
import System.Directory
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError)

data Paths = Paths
  { runsDir :: FilePath
  , -- | ホストごとのロックファイル（'Meguri.Fetch.Http.takeTurn'）。使っている最中に消すと順番が守れないので、掃除しない
    locksDir :: FilePath
  , -- | 古い版が残したもの（応答と判定の答えのキャッシュ、直前の実行を指すファイル）。掃除で消す
    leftovers :: [FilePath]
  }

getPaths :: IO Paths
getPaths = do
  base <- getXdgDirectory XdgCache "meguri"
  pure
    Paths
      { runsDir = base </> "runs"
      , locksDir = base </> "locks"
      , leftovers = [base </> "http", base </> "judge", base </> "runs" </> "latest"]
      }

-- | 実行の記録を置くフォルダを作る。名前は日本時間の日時で、同じ秒にほかの実行があれば -2・-3 と続ける。
-- フォルダを作れたプロセスだけがその名前を使うので、同時に走っても同じフォルダに書かない。
newRun :: Paths -> IO (Text, FilePath)
newRun p = do
  now <- getCurrentTime
  createDirectoryIfMissing True p.runsDir
  let stamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" (addUTCTime (9 * 3600) now)
  claim (stamp : [stamp <> "-" <> show n | n <- [2 :: Int ..]])
 where
  claim names = case names of
    [] -> panic "名前の候補は尽きない"
    rid : rest -> do
      made <- try (createDirectory (p.runsDir </> rid))
      case made of
        Right () -> pure (toS rid, p.runsDir </> rid)
        Left e
          | isAlreadyExistsError e -> claim rest
          | otherwise -> throwIO e

-- | 7日より古い実行の記録と、古い版が残したものを消す。
-- ほかの meguri が同じものを先に消していてもよい。消せなかったものは次に回し、コマンドは止めない。
purgeOld :: Paths -> IO ()
purgeOld p = do
  now <- getCurrentTime
  for_ p.leftovers (quietly . removePathForcibly)
  names <- either (const []) identity <$> (try (listDirectory p.runsDir) :: IO (Either IOException [FilePath]))
  for_ names $ \n -> quietly $ do
    let path = p.runsDir </> n
    isDir <- doesDirectoryExist path
    when isDir $ do
      t <- getModificationTime path
      when (diffUTCTime now t > 7 * 86400) (removePathForcibly path)
 where
  quietly act = void (try act :: IO (Either IOException ()))

-- | JSON Lines の1行。版（schema）を付ける。
line :: ToJSON a => Text -> a -> ByteString
line schema a = BL.toStrict . encode $ case toJSON a of
  Object o -> Object (KM.insert "schema" (String schema) o)
  v -> v

writeLines :: FilePath -> [ByteString] -> IO ()
writeLines path ls = unless (null ls) $ BS.writeFile path (BC.unlines ls)
