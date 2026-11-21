-- | ブックオフ オンライン
module Meguri.Site.BookOff (adapter) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.Price
import Meguri.Site.Common
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

freeOver :: Shipping
freeOver = ShipUnknown "1,800円以上のご注文で送料無料（未満の送料はご利用ガイドで確かめる）"

listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html =
  let os = mapMaybe one (subtrees (hasClass "productItem") ts)
   in ListOut
        { offers = os
        , zero = null os && textHas ["見つかりませんでした", "該当する商品はありません", "0件"] html
        , follow = []
        }
 where
  one el = do
    href <- attr "href" =<< find (hasClass "productItem__link") el
    let img = find (isJust . attr "alt") el
        tagTexts = mapMaybe (firstTextWhere (const True)) (subtrees (hasClass "tag") el)
        stockText = textOfClass "productItem__stock" el
        inStock = any (hasClass "jsBtn-cart") el || maybe False (textHas ["残り", "在庫あり"]) stockText
    pure
      (listOffer ctx (absUrl ctx.pageUrl href))
        { name = slMaybe "商品名が読めなかった" (textOfClass "productItem__title" el)
        , priceIncl = slMaybe "価格が読めなかった" (parseYen =<< textOfClass "productItem__price" el)
        , shipping = missing SearchList "一覧に送料の表示なし（1,800円以上で無料）"
        , stock = (if inStock then sl InStock else sl OutOfStock) {raw = stockText}
        , seller = sl "ブックオフ オンライン"
        , image = slMaybe "画像なし" (attr "src" =<< img)
        , condition = slMaybe "新品・中古の表示なし" (find (`elem` ["中古", "新品"]) tagTexts)
        , keys = noKeys {siteItemId = Just (T.takeWhileEnd (/= '/') href)}
        }

itemPage :: ItemCtx -> Offer Method -> Offer Method
itemPage ctx d =
  d
    { priceIncl = case metaContent "description" ctx.tags of
        Just desc
          | Just y <- d.priceIncl.value
          , T.filter (/= ',') (show (unYen y) <> "円(税込)") `T.isInfixOf` T.filter (/= ',') desc ->
              d.priceIncl {note = Just "説明文の「円(税込)」と一致"}
        _ -> d.priceIncl {note = Just "JSON-LD の価格"}
    , shipping =
        if textHas ["1,800円以上のご注文で送料無料"] ctx.html
          then gotRaw Body "1,800円以上のご注文で送料無料" freeOver
          else missing Body "送料の表示なし"
    , seller = got Body "ブックオフ オンライン"
    , buy = case (any (hasClass "jsBtn-cart") ctx.tags, d.stock.value) of
        (True, _) -> got Body (BuyActive "カートに入れる")
        (False, Just OutOfStock) -> got Body (BuyDisabled "在庫なし")
        _ -> missing Body "カートのボタンが無い"
    , keys = d.keys {siteItemId = Just (T.takeWhileEnd (/= '/') (stripQuery ctx.pageUrl))}
    }
