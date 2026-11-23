-- | 個別ページで確かめる：対象を選び、ページを読み、一覧の値との違いを出す
module Meguri.Read
  ( Page (..)
  , named
  , Checked (..)
  , Unread (..)
  , cheapest
  , tracked
  , readPages
  , driftFrom
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Meguri.Fetch.Http (Fetcher)
import Meguri.Item
import Meguri.Render (cheapestFirst, shippingShort, stockShort, yen)
import Meguri.Ship (Pref)
import Meguri.Types
import Protolude

-- | 読む個別ページ
data Page = Page
  { url :: Text
  , -- | 一覧で見かけたもの。'Nothing' なら、名指しで渡されたページ（--url）
    sighting :: Maybe Sighting
  }

-- | 名指しで渡されたページか。名指しのページは、選び方（accept）で外さない
named :: Page -> Bool
named p = isNothing p.sighting

-- | 個別ページを読めたもの
data Checked = Checked
  { page :: Page
  , reading :: Reading
  , -- | 一覧の値と違っていたところ
    drift :: [Text]
  , -- | 取得の並び：個別ページと、送料を問い合わせたならその問い合わせ
    attempts :: [Attempt]
  }

-- | 個別ページを読めなかったもの
data Unread = Unread
  { page :: Page
  , -- | 取りに行かなかった（HTTP では読めない販路・価格.com の転送URL）か、取りに行って読めなかった（その取得）
    did :: Did
  }

-- | 読む個別ページを選ぶ：販路と新品・中古の組ごとに、安い順で n 件。
-- 同じURLと、同じ在庫（共通の在庫番号）は1つに数える。
cheapest :: Int -> [Sighting] -> [Page]
cheapest n = go Map.empty . cheapestFirst
 where
  go _ [] = []
  go seen (c : cs) =
    let k = (c.offer.site, c.market)
        used = Map.findWithDefault (0 :: Int) k seen
     in if used < n
          then Page c.offer.url (Just c) : go (Map.insert k (used + 1) seen) cs
          else go seen cs

-- | 価格.com の転送URL。アフィリエイトの中継を通るので、自動では取りに行かない
tracked :: Text -> Bool
tracked u = any (`T.isPrefixOf` u) ["https://c.kakaku.com/forwarder/", "http://c.kakaku.com/forwarder/", "https://kakaku.com/ksearch/jump/"]

-- | 対象の個別ページを、並列に読む（同じホストへは1本ずつ・2秒おき。"Meguri.Fetch.Http" が守る）。
-- 同じホストの中は、渡した順（安い順）に読む。期限までに読めなかったものは、その取得が「時間切れ」になる。
readPages :: Fetcher -> Maybe Pref -> Maybe UTCTime -> [Page] -> IO ([Checked], [Unread])
readPages f pref deadline pages = partitionEithers <$> mapConcurrently one pages
 where
  one p
    | tracked p.url =
        pure (Right (Unread p (Skipped (NeedsBrowser "価格.com の転送URL（アフィリエイトの中継を通る）なので、取りに行かない"))))
    | otherwise = do
        r <- readItemShipped f deadline pref p.url
        pure $ case r of
          ItemRead reading as -> Left (Checked p reading (maybe [] (`driftFrom` reading) p.sighting) as)
          ItemSkipped why -> Right (Unread p (Skipped why))
          ItemUnread as -> Right (Unread p (Tried as))

-- | 一覧の値と、個別ページの値の違い（個別ページの方を採る）
driftFrom :: Sighting -> Reading -> [Text]
driftFrom c r =
  catMaybes
    [ case (c.offer.priceIncl.value, r.offer.priceIncl.value) of
        (Just a, Just b) | a /= b -> Just ("価格 " <> yen a <> "→" <> yen b <> "（一覧→個別ページ）")
        _ -> Nothing
    , case (c.offer.stock.value, r.offer.stock.value) of
        (Just a, Just b) | stockKind a /= stockKind b -> Just ("在庫 " <> stockShort (Just a) <> "→" <> stockShort (Just b))
        _ -> Nothing
    , case (c.offer.shipping.value, r.offer.shipping.value) of
        (Just a, Just b) | known a && known b && a /= b -> Just ("送料 " <> shippingShort (Just a) <> "→" <> shippingShort (Just b))
        _ -> Nothing
    ]
 where
  -- オークションの入札数や終了時刻の違いは「変化」に数えない
  stockKind :: Stock -> Int
  stockKind = \case
    InStock -> 0
    Selling -> 0
    Backorder -> 1
    OutOfStock -> 2
    Sold -> 2
    Auction _ _ -> 3
    StockUnknown -> 4
  known = \case
    ShipUnknown _ -> False
    ByDestination -> False
    BuyerPays -> False
    _ -> True
