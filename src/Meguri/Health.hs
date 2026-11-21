-- | 健康診断の結果（記録）。結果そのものは、`meguri check --write` が
-- "Meguri.Catalog.Health" に書く。ここは、その型と、書き出し方。
module Meguri.Health
  ( Health (..)
  , recorded
  , healthOf
  , moduleSource
  , parseModule
  , merge
  ) where

import Data.Text qualified as T
import Meguri.Catalog.Health qualified as Generated
import Meguri.Text (clean)
import Protolude

data Health = Health
  { sid :: Text
  , -- | 診断した日（JST）
    day :: Text
  , -- | 検索の結果（「検索：取得45件」など）
    search :: Text
  , -- | 個別ページの結果（「個別：確認済み」など）
    item :: Text
  , -- | カタログの段のとおりだったか（False なら、カタログを見直す）
    asExpected :: Bool
  }
  deriving stock (Eq, Show)

-- | ビルドに入っている記録
recorded :: [Health]
recorded = [Health a b c d e | (a, b, c, d, e) <- Generated.rows]

healthOf :: [Health] -> Text -> Maybe Health
healthOf hs i = find ((== i) . (.sid)) hs

-- | 新しい結果で、同じサイトの古い結果を置き換える（順番は、新しい結果の順 → 残りの古い結果）
merge :: [Health] -> [Health] -> [Health]
merge new old = new <> [h | h <- old, h.sid `notElem` map (.sid) new]

-- | "Meguri.Catalog.Health" のソース
moduleSource :: [Health] -> Text
moduleSource hs =
  T.unlines $
    [ "-- このファイルは `meguri check --write` が作る。手で直さない。"
    , "-- 中身は、サイトごとの直近の健康診断の結果（id・日付・検索・個別・段のとおりか）。"
    , "module Meguri.Catalog.Health (rows) where"
    , ""
    , "import Protolude"
    , ""
    , "rows :: [(Text, Text, Text, Text, Bool)]"
    , "rows ="
    ]
      <> case hs of
        [] -> ["  []"]
        _ -> zipWith row ("  [ " : repeat "  , ") hs <> ["  ]"]
 where
  row lead h = lead <> "(" <> T.intercalate ", " [lit h.sid, lit h.day, lit h.search, lit h.item, if h.asExpected then "True" else "False"] <> ")"
  -- Haskell の文字列。日本語はそのまま書き、\ と " だけを逃がす（制御文字は 'clean' が除く）
  lit t = "\"" <> T.concatMap esc (clean 200 t) <> "\""
  esc c = case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    _ -> T.singleton c

-- | 'moduleSource' で書いたソースから、記録を読み戻す。
-- 一部のサイトだけ診断して書き足すとき、ビルドに入っている記録ではなく、ディスク上の記録と混ぜるために使う。
parseModule :: Text -> [Health]
parseModule src = mapMaybe row (T.lines src)
 where
  row l = do
    body <- T.stripPrefix "  [ " l <|> T.stripPrefix "  , " l
    (a, b, c, d, e) <- readMaybe (toS body :: [Char]) :: Maybe ([Char], [Char], [Char], [Char], Bool)
    pure (Health (toS a) (toS b) (toS c) (toS d) e)
