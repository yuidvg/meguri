-- | サイトによらない抽出。アダプタは、この結果にサイトの癖だけを上書きする。
module Meguri.Extract.Generic
  ( Draft
  , genericDraft
  , cheapestOffer
  ) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd
import Meguri.Extract.Microdata
import Meguri.Extract.OpenGraph
import Meguri.Extract.Price
import Meguri.Text (digitsOnly)
import Meguri.Types
import Protolude

-- | 個別ページから読んだ値の下書き。出どころは「ページのどこから読んだか」。
type Draft = Offer Method

-- | 価格のある offer のうち、いちばん安いもの
cheapestOffer :: LdProduct -> Maybe LdOffer
cheapestOffer p = head (sortOn (.price) (filter (isJust . (.price)) p.offers)) <|> head p.offers

-- | JSON-LD → microdata → OGP の順に読む。
--
-- JSON-LD の価格が税込かどうかは、サイトごとに違う。ここでは税込の欄に入れ、
-- 注記を付ける。税抜のサイト（ハードオフなど）はアダプタで直す。
genericDraft :: Text -> Text -> Text -> Tags -> Draft
genericDraft site url at ts =
  (emptyOffer Body site url at)
    { name =
        fromMaybeF JsonLd (ld >>= (.name))
          `orField` fromMaybeF Microdata (itemprop "name" ts)
          `orField` fromMaybeF OpenGraph (ogTitle ts)
          `orField` fromMaybeF Body (titleText ts)
    , priceIncl =
        withNote "税込かどうかは未確認（汎用の抽出）" $
          fromMaybeF JsonLd (Yen <$> (offer >>= (.price)))
            `orField` fromMaybeF Microdata (parseYen =<< itemprop "price" ts)
            `orField` fromMaybeF OpenGraph (ogPriceAmount ts)
    , shipping = fromMaybeF JsonLd ((\v -> if v == 0 then Free else Fixed (Yen v)) <$> (offer >>= (.shippingValue)))
    , stock =
        fromMaybeRaw JsonLd (offer >>= (.availability)) stockFromSchema
          `orField` fromMaybeRaw Microdata (itemprop "availability" ts) stockFromSchema
    , seller = fromMaybeF JsonLd (offer >>= (.sellerName))
    , image =
        fromMaybeF JsonLd (ld >>= head . (.images))
          `orField` fromMaybeF Microdata (itemprop "image" ts)
          `orField` fromMaybeF OpenGraph (ogImage ts)
    , condition = fromMaybeRaw JsonLd (offer >>= (.itemCondition)) conditionFromSchema
    , keys =
        Keys
          { jan = validJan =<< ((ld >>= (.gtin)) <|> itemprop "gtin13" ts)
          , model = ld >>= (.mpn)
          , siteItemId = (ld >>= (.sku)) <|> itemprop "sku" ts
          , sharedStockId = Nothing
          }
    }
 where
  ld = ldProduct ts
  offer = ld >>= cheapestOffer
  validJan t = let d = digitsOnly t in if T.length d `elem` [8, 13] then Just d else Nothing

fromMaybeF :: Method -> Maybe a -> Field Method a
fromMaybeF m = maybe (missing m "読めなかった") (got m)

fromMaybeRaw :: Method -> Maybe Text -> (Text -> Maybe a) -> Field Method a
fromMaybeRaw m mraw f = case mraw of
  Nothing -> missing m "読めなかった"
  Just r -> case f r of
    Just a -> gotRaw m r a
    Nothing -> (missing m "値を解釈できなかった") {raw = Just r}

conditionFromSchema :: Text -> Maybe Text
conditionFromSchema u
  | "NewCondition" `T.isSuffixOf` u = Just "新品"
  | "UsedCondition" `T.isSuffixOf` u = Just "中古"
  | "RefurbishedCondition" `T.isSuffixOf` u = Just "整備済み"
  | "DamagedCondition" `T.isSuffixOf` u = Just "難あり"
  | otherwise = Nothing
