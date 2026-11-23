-- | Yahoo!フリマ（旧PayPayフリマ）
module Meguri.Site.PayPayFleamarket (adapter) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (at, atPath, str)
import Meguri.Extract.NextData (nextData)
import Meguri.Extract.Price (parseYen)
import Meguri.Site.Common
import Meguri.Types
import Protolude
import Text.HTML.TagSoup (Tag (..))

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

-- | 一覧の商品は @<a href="/item/z…">@ にある（__NEXT_DATA__ ではない）。
-- 一致する商品が無いと、HTTP 404 で「検索キーワードに近い商品」を並べる。探した物ではないので、0件とする
listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html
  | textHas ["一致する商品が見つからなかったため"] html = ListOut [] True []
  | otherwise =
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
    , stock = case status of
        Just "OPEN" -> gotRaw NextData "status:OPEN" Selling
        Just "SOLD" -> gotRaw NextData "status:SOLD" Sold
        Just other -> gotRaw NextData ("status:" <> other) OutOfStock
        Nothing
          | hasOrder -> gotRaw Body "購入手続きへ" Selling
          | soldOut -> gotRaw Body "売り切れ" Sold
          | otherwise -> d.stock
    , buy = buyEntry
    , shipping = d.shipping `orField` (if textHas ["ItemDetail__ShippingFreeLabel", "送料無料"] ctx.html then gotRaw Body "送料無料" Free else missing Body "送料の表示なし")
    , seller = bodyField Body "出品者名が読めなかった" sellerName
    , image = fmap (T.takeWhile (/= '?')) d.image
    , keys = let zid = T.takeWhileEnd (/= '/') (stripQuery ctx.pageUrl) in d.keys {siteItemId = Just zid, sharedStockId = Just zid}
    }
 where
  -- 出品の記録（__NEXT_DATA__）の状態。OPEN（販売中）・SOLD（売り切れ）など
  status = str =<< at "status" =<< atPath ["props", "initialState", "itemsState", "items", "item"] =<< nextData ctx.tags
  hasOrder = any (\t -> maybe False ("/order" `T.isSuffixOf`) (attr "href" t)) ctx.tags
  soldOut = textHas ["売り切れました", "SOLD OUT", ">売り切れ<"] ctx.html
  -- ウェブに購入ボタンが無く、アプリで開くボタンだけが出る出品がある（2026-11-23、おまかせ配送の洗濯機で見た）
  appOnly = textHas ["この商品をアプリで見る"] ctx.html
  buyEntry
    | hasOrder = got Body (BuyActive "購入手続きへ")
    | status == Just "OPEN" && appOnly = withNote "ウェブの購入ボタンは無く、アプリで開くボタンだけ" (got Body (BuyActive "この商品をアプリで見る"))
    | Just st <- status, st /= "OPEN" = gotRaw NextData ("status:" <> st) (BuyDisabled (if st == "SOLD" then "売り切れ" else "販売中でない"))
    | soldOut = got Body (BuyDisabled "売り切れ")
    | otherwise = missing Body "購入ボタンが無い"
  -- 出品者の欄（/user/ へのリンク）の最初の文字
  sellerName = do
    el <- head (subtrees (\t -> maybe False (\h -> "/user/" `T.isInfixOf` h) (attr "href" t)) ctx.tags)
    find (not . T.null) [T.strip t | TagText t <- el]
