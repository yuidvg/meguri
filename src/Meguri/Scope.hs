-- | どこを探すか：当たる販路の選び方、新品・中古、名指しのページ。
--
-- search の形は「何を（"Meguri.Query"）→ どこを（このモジュール）→ 結果」。
--
-- * 販路は、カタログから選ぶ。条件（id・区分）と、判定（外のコマンド・jev）が使える。
--   「この品物を、この店は扱うか」は、判定がする。
-- * 名指しのページは、販路の検索とは別に「ここも見る」もの。検索がJSで描画される販路で、ブラウザで見つけた商品など。
module Meguri.Scope
  ( Scope (..)
  , only
  , skip
  , categoryHas
  , judgeByCommand
  , judgeByJev
  , unknownChannels
  , channelRows
  , channelsIn
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text qualified as T
import Meguri.Catalog
import Meguri.Read (Page)
import Meguri.Selection
import Meguri.Text (clean)
import Meguri.Types (Market)
import Protolude

data Scope = Scope
  { -- | 当たる販路の選び方（カタログから選ぶ）
    channels :: Selection Channel
  , -- | 新品・中古（空なら両方）
    markets :: [Market]
  , -- | 名指しのページ
    pages :: [Page]
  }

-- | この id の販路だけ
only :: [Text] -> Criterion Channel
only ids = Criterion ("--only " <> T.intercalate "," ids) (\c -> c.sid `elem` ids)

-- | この id の販路を除く
skip :: [Text] -> Criterion Channel
skip ids = Criterion ("--skip " <> T.intercalate "," ids) (\c -> c.sid `notElem` ids)

-- | 区分の名前に、どれかの語を含む販路だけ
categoryHas :: [Text] -> Criterion Channel
categoryHas ws = Criterion ("--category " <> T.intercalate "," ws) (\c -> any (`T.isInfixOf` categoryName c.category) ws)

-- | 外のコマンドに判定を任せる
judgeByCommand :: Text -> Judge Channel
judgeByCommand cmd = Judge ("--judge-channel-cmd " <> clean 40 cmd) (Command cmd)

-- | jev に判定を任せる（はい／いいえの質問。「はい」なら当たる）
judgeByJev :: Text -> Judge Channel
judgeByJev question = Judge ("--judge-channel " <> clean 40 question) (Jev question)

-- | カタログに無い id（書きまちがいを、0件の結果にしないために見る）
unknownChannels :: [Text] -> [Text]
unknownChannels = filter (isNothing . lookupSite)

-- | 販路を、判定に渡す行にする。id は販路の id。検索語も入れる（何を探しているかが分かるように）
channelRows :: [Text] -> [Channel] -> [(Text, Value)]
channelRows terms = map row
 where
  row c =
    ( c.sid
    , object
        [ "id" .= c.sid
        , "name" .= c.label
        , "category" .= categoryName c.category
        , "sells" .= map kindLabel c.kinds
        , "reach" .= tierLabel c.tier
        , "terms" .= terms
        ]
    )

-- | カタログから、当たる販路を選ぶ。判定が失敗したら、そこでやめて理由を返す
channelsIn :: Ask -> [Text] -> Scope -> IO (Either Text (Selected Channel))
channelsIn ask terms scope = select ask (channelRows terms) scope.channels identity catalog
