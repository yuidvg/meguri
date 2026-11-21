-- | 何を探すか：検索語と、見つけた売り物のうち「探している物」と認める選び方。
--
-- 選び方（'accept'）は、結果を後から削るものではない。どの個別ページを読むかも、これで決まる。
--
-- * 条件（価格・商品名）は、一覧の値にかけ、個別ページを読んだあとの値にもう一度かける。
-- * 判定（外のコマンド・jev）は、一覧の値に1回だけかける。「これは探している物か、付属品か」は、判定がする。
module Meguri.Query
  ( Query (..)
  , Seen (..)
  , ofSighting
  , ofReading
  , minPrice
  , maxPrice
  , nameHas
  , nameLacks
  , judgeByCommand
  , judgeByJev
  , seenRows
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text qualified as T
import Meguri.Render (shippingShort, stockShort)
import Meguri.Selection
import Meguri.Text (clean, toHalfWidth)
import Meguri.Types
import Protolude

data Query = Query
  { -- | 販路の検索窓に入れる語（型番・JAN・商品名など）
    terms :: [Text]
  , -- | 見つけた売り物のうち、探している物と認める選び方
    accept :: Selection Seen
  }

-- | 売り物の見え方。一覧で見た値からも、個別ページで読んだ値からも、同じ形にする
data Seen = Seen
  { site :: Text
  , market :: Maybe Market
  , term :: Maybe Text
  , name :: Maybe Text
  , -- | 税込価格。無ければ税抜価格
    price :: Maybe Yen
  , payable :: Maybe Yen
  , shipping :: Maybe Shipping
  , stock :: Maybe Stock
  , seller :: Maybe Text
  , condition :: Maybe Text
  , url :: Text
  }

ofOffer :: Maybe Market -> Maybe Text -> Offer s -> Seen
ofOffer market term o =
  Seen
    { site = o.site
    , market
    , term
    , name = o.name.value
    , price = o.priceIncl.value <|> o.priceExcl.value
    , payable = payable o
    , shipping = o.shipping.value
    , stock = o.stock.value
    , seller = o.seller.value
    , condition = o.condition.value
    , url = o.url
    }

ofSighting :: Sighting -> Seen
ofSighting s = ofOffer (Just s.market) (Just s.term) s.offer

-- | 個別ページで読んだ値。一覧で見かけたものが分かれば、検索語と新品・中古を引き継ぐ
ofReading :: Maybe Sighting -> Reading -> Seen
ofReading sighting r = ofOffer ((.market) <$> sighting) ((.term) <$> sighting) r.offer

-- | 価格が分からないものは残す（個別ページで分かってから、もう一度かかる）
minPrice :: Int -> Criterion Seen
minPrice n = Criterion ("--min-price " <> show n) (\s -> maybe True (>= Yen n) s.price)

maxPrice :: Int -> Criterion Seen
maxPrice n = Criterion ("--max-price " <> show n) (\s -> maybe True (<= Yen n) s.price)

-- | 商品名に語を含む（大文字小文字・全角半角・空白・ハイフンの違いは無視）
nameHas :: Text -> Criterion Seen
nameHas w = Criterion ("--has " <> w) (\s -> maybe False (contains w) s.name)

-- | 商品名に語を含まない
nameLacks :: Text -> Criterion Seen
nameLacks w = Criterion ("--not " <> w) (\s -> maybe True (not . contains w) s.name)

contains :: Text -> Text -> Bool
contains w n = fold w `T.isInfixOf` fold n
 where
  fold = T.toLower . T.filter (\c -> not (isSpace c) && c /= '-') . toHalfWidth

-- | 外のコマンドに判定を任せる
judgeByCommand :: Text -> Judge Seen
judgeByCommand cmd = Judge ("--judge-cmd " <> clean 40 cmd) (Command cmd)

-- | jev に判定を任せる（はい／いいえの質問。「はい」なら残す）
judgeByJev :: Text -> Judge Seen
judgeByJev question = Judge ("--judge " <> clean 40 question) (Jev question)

-- | 売り物を、判定に渡す行にする。id は、この呼び出しの中だけの番号
seenRows :: [Seen] -> [(Text, Value)]
seenRows = zipWith row [0 :: Int ..]
 where
  row i s =
    ( show i
    , object
        [ "id" .= i
        , "site" .= s.site
        , "market" .= fmap marketText s.market
        , "term" .= s.term
        , "name" .= s.name
        , "price" .= fmap unYen s.price
        , "payable" .= fmap unYen s.payable
        , "shipping" .= fmap (shippingShort . Just) s.shipping
        , "stock" .= fmap (stockShort . Just) s.stock
        , "seller" .= s.seller
        , "condition" .= s.condition
        , "url" .= s.url
        ]
    )
  marketText :: Market -> Text
  marketText = \case
    New -> "new"
    Used -> "used"
