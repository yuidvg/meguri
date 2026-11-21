-- | Yahoo!フリマ（旧PayPayフリマ）
module Meguri.Site.PayPayFleamarket (adapter) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.Price (parseYen)
import Meguri.Site.Common
import Meguri.Types
import Protolude
import Text.HTML.TagSoup (Tag (..))

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

-- | 一覧の商品は @<a href="/item/z…">@ にある（__NEXT_DATA__ ではない）
listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html =
  ListOut
    { offers = mapMaybe one (subtrees isItemLink ts)
    , zero = textHas ["に一致する商品はありません", "見つかりませんでした", "商品が見つかりません"] html
    , follow = []
    }
 where
  isItemLink t = maybe False ("/item/z" `T.isPrefixOf`) (attr "href" t)
  one el = do
    href <- attr "href" =<< head el
    img <- find (isJust . attr "alt") el
    let priceText = find ("円" `T.isSuffixOf`) (map T.strip (T.lines (T.replace "円" "円\n" (innerText el))))
    pure
      (listOffer ctx ("https://paypayfleamarket.yahoo.co.jp" <> href))
        { name = slMaybe "商品名が読めなかった" (attr "alt" img)
        , priceIncl = slMaybe "価格が読めなかった" (parseYen =<< priceText)
        , shipping = withNote "Yahoo!フリマは全品送料無料" (sl Free)
        , stock = withNote "販売中で絞った検索" (sl Selling)
        , seller = missing SearchList "一覧に出品者名なし"
        , image = slMaybe "画像なし" (T.takeWhile (/= '?') <$> attr "src" img)
        , keys = let zid = T.drop (T.length "/item/") href in noKeys {siteItemId = Just zid, sharedStockId = Just zid}
        }

itemPage :: ItemCtx -> Offer Method -> Offer Method
itemPage ctx d =
  d
    { priceIncl = d.priceIncl {note = Just "フリマの表示価格（これが支払う額）"}
    , stock = case (hasOrder, soldOut) of
        (True, _) -> gotRaw Body "購入手続きへ" Selling
        (False, True) -> gotRaw Body "売り切れ" Sold
        _ -> d.stock
    , buy = if hasOrder then got Body (BuyActive "購入手続きへ") else if soldOut then got Body (BuyDisabled "売り切れ") else missing Body "購入ボタンが無い"
    , shipping = d.shipping `orField` (if textHas ["ItemDetail__ShippingFreeLabel", "送料無料"] ctx.html then gotRaw Body "送料無料" Free else missing Body "送料の表示なし")
    , seller = bodyField Body "出品者名が読めなかった" sellerName
    , image = fmap (T.takeWhile (/= '?')) d.image
    , keys = let zid = T.takeWhileEnd (/= '/') (stripQuery ctx.pageUrl) in d.keys {siteItemId = Just zid, sharedStockId = Just zid}
    }
 where
  hasOrder = any (\t -> maybe False ("/order" `T.isSuffixOf`) (attr "href" t)) ctx.tags
  soldOut = textHas ["売り切れました", "SOLD OUT", ">売り切れ<"] ctx.html
  -- 出品者の欄（/user/ へのリンク）の最初の文字
  sellerName = do
    el <- head (subtrees (\t -> maybe False (\h -> "/user/" `T.isInfixOf` h) (attr "href" t)) ctx.tags)
    find (not . T.null) [T.strip t | TagText t <- el]
