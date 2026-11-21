-- | 確認済みかどうかを計算で決める。
--
-- 受け取るのは個別ページの値（@Offer OnPage@）だけ。
-- 一覧やAPIの値（@Offer OnList@）からは 'Confirmed' を作れない。
module Meguri.Confirm
  ( toReading
  , judge
  , missingFields
  ) where

import Meguri.Extract.Generic (Draft)
import Meguri.Types
import Protolude

-- | 下書きに「HTTPで取った／ブラウザで保存した」の別を付け、判定を添える
toReading :: PageOrigin -> Bool -> Draft -> Reading
toReading origin adapter d =
  let wrap = case origin of
        FromHttp -> ItemPage
        FromCapture -> Captured
      f :: Field Method a -> Field OnPage a
      f = mapSource wrap
      o =
        Offer
          { site = d.site
          , url = d.url
          , name = f d.name
          , priceIncl = f d.priceIncl
          , priceExcl = f d.priceExcl
          , shipping = f d.shipping
          , stock = f d.stock
          , seller = f d.seller
          , image = f d.image
          , buy = f d.buy
          , points = f d.points
          , condition = f d.condition
          , keys = d.keys
          , at = d.at
          }
   in Reading {offer = o, adapter, confirmation = judge adapter o}

-- | 名前・税込価格・送料・在庫・販売者・画像・購入の入口が、すべて読めたら確認済み
judge :: Bool -> Offer OnPage -> Confirmation
judge adapter o = case missingFields o <> ["アダプタ（汎用の抽出だけ）" | not adapter] of
  [] -> Confirmed
  ms
    | length (missingFields o) == 7 -> Unconfirmed
    | otherwise -> Partial ms

missingFields :: Offer OnPage -> [Text]
missingFields o =
  concat
    [ ["名前" | isNothing o.name.value]
    , ["税込価格" | isNothing o.priceIncl.value]
    , ["送料" | not shippingRead]
    , ["在庫" | not stockRead]
    , ["販売者" | isNothing o.seller.value]
    , ["画像" | isNothing o.image.value]
    , ["購入の入口" | isNothing o.buy.value]
    ]
 where
  shippingRead = case o.shipping.value of
    Nothing -> False
    Just (ShipUnknown _) -> False
    Just _ -> True
  stockRead = case o.stock.value of
    Nothing -> False
    Just StockUnknown -> False
    Just _ -> True
