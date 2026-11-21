-- | 店や出品者が入れ替わる売り場のアダプタ（LOHACO・au PAY マーケット・BUYMA・ロコンド）。
--
-- 販売者は商品ごとに違う。ページに書いてある店・出品者の名前を読む。
module Meguri.Site.Malls
  ( lohaco
  , aupay
  , buyma
  , locondo
  ) where

import Data.Aeson (Value (..))
import Data.Text qualified as T
import Meguri.Catalog (lookupSite)
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (LdOffer (..), LdProduct (..), arr, at, atPath, int, ldProduct, str)
import Meguri.Extract.NextData (nextData)
import Meguri.Extract.OpenGraph (ogTitle)
import Meguri.Extract.Price (parseShipping, parseYen, stockFromSchema)
import Meguri.Site.Common
import Meguri.Site.Store
import Meguri.Text (digitsOnly)
import Meguri.Types
import Protolude
import Text.HTML.TagSoup (Tag (..))

-- | URL の最後の区切り（? より前）
lastSegment :: Text -> Text
lastSegment = T.takeWhileEnd (/= '/') . T.dropWhileEnd (== '/') . stripQuery

byId :: Text -> Tags -> Maybe Tags
byId i ts = head (subtrees (\t -> attr "id" t == Just i) ts)

-- LOHACO ---------------------------------------------------------------------

-- | 値は、h1 から「こちらの商品もおすすめ」の見出しまでの本文にある。
-- クラス名はビルドごとに変わるので使わない。価格は「61」「円」「（税込）」と、3つの要素に分かれて並ぶ。
-- 検索はJSで描画されるので、一覧は読まない。
lohaco :: Adapter
lohaco = Adapter Nothing $ Just $ \ctx d ->
  let main = takeWhile (not . recommend) (dropWhile (not . isH1) ctx.tags)
      texts = visibleTexts main
      c = compact (T.unwords texts)
      price = taxInclAmount texts
      -- 「この商品は」「LOHACO」「が販売・発送します。」
      sellerName =
        head [b | (a : b : rest : _) <- tails texts, a == "この商品は", "が販売" `T.isPrefixOf` rest]
          <|> (let (_, hit) = T.breakOn "この商品は" c in mfilter (not . T.null) (Just (fst (T.breakOn "が販売" (T.drop 5 hit)))))
      -- 「…が販売・発送します。最短翌日お届け3,780円(税込)以上で基本配送料550円(税込)無料 配送料について」
      shipText =
        let (_, hit) = T.breakOn "発送します。" c
         in mfilter (T.isInfixOf "送料") (Just (T.take 80 (fst (T.breakOn "配送料について" (T.drop 6 hit)))))
      stockText = find (\t -> T.length t <= 10 && textHas ["在庫", "入荷", "販売終了", "売り切れ", "品切れ"] t) texts
      stock = stockField "在庫の表示なし" stockText
   in d
        { name = bodyField Body "商品名（h1）が無い" (mfilter (not . T.null) (Just (visibleText (subtree (dropWhile (not . isH1) ctx.tags))))) `orField` d.name
        , priceIncl = case price of
            Just y -> gotRaw Body (show (unYen y) <> "円（税込）") y
            Nothing -> missing Body "「◯円（税込）」の並びが無い"
        , shipping = case shipText of
            Just t | Just s <- parseShipping t -> withNote "一部の商品は、基本配送料がかからない" (gotRaw Body t s)
            Just t -> (missing Body "配送料の表示を解釈できなかった") {raw = Just t}
            Nothing -> missing Body "配送料の表示なし"
        , stock
        , seller = bodyField Body "「この商品は◯◯が販売・発送します」の表示なし" sellerName
        , buy = case (buttonWith "カートに入れる" main, stock.value) of
            (Just (BuyActive l), Just OutOfStock) -> withNote "在庫なしなので、押せないはず" (got Body (BuyDisabled l))
            (Just b, _) -> got Body b
            (Nothing, _) -> missing Body "カートのボタンが見つからない"
        , keys = d.keys {siteItemId = mfilter (not . T.null) (Just (lastSegment ctx.pageUrl))}
        }
 where
  isH1 = \case
    TagOpen "h1" _ -> True
    _ -> False
  recommend = \case
    TagText t -> "こちらの商品もおすすめ" `T.isInfixOf` t
    _ -> False

-- au PAY マーケット ------------------------------------------------------------

-- | 値は __NEXT_DATA__（initialState.itemDetails.itemPageInfo）にある。
-- 送料とカートのボタンはJSで描画されるので、HTTP で取ったHTMLからは読めない。
-- 検索もJSで描画されるので、一覧は読まない。
aupay :: Adapter
aupay = Adapter Nothing $ Just $ \ctx d ->
  let info = atPath ["props", "pageProps", "initialState", "itemDetails", "itemPageInfo"] =<< nextData ctx.tags
      item = at "itemInfo" =<< info
      get k = at k =<< item
      flag ks = (atPath ks =<< item) == Just (Bool True)
      price = int =<< get "currentPrice"
      title = str =<< get "itemTitle"
      remaining = int =<< get "remainingItemCount"
      closed = get "isStatusOpen" == Just (Bool False) || flag ["itemStatus", "isShopClose"] || flag ["itemStatus", "isItemNotFound"]
      shop = (str =<< atPath ["shopSummaryInfo", "shopName"] =<< info) <|> (str =<< get "shopNameForCell")
      image = str =<< head . arr =<< get "productImageUrls"
   in d
        { name = bodyField NextData "商品名（itemTitle）が無い" title `orField` d.name
        , priceIncl = case price of
            Just p
              | flag ["taxTypes", "isInclusiveTax"] -> (got NextData (Yen p)) {note = Just "税込の印（taxTypes.isInclusiveTax）あり"}
              | flag ["taxTypes", "isExclusiveTax"] -> missing NextData "税抜の表示（taxTypes.isExclusiveTax）"
              | otherwise -> (missing NextData "税込かどうかの印（taxTypes）が無い") {raw = Just (show p)}
            Nothing -> missing NextData "価格（currentPrice）が無い"
        , priceExcl = case price of
            Just p | flag ["taxTypes", "isExclusiveTax"] -> got NextData (Yen p)
            _ -> missing NextData "税抜の表示なし"
        , stock = case remaining of
            _ | closed -> gotRaw NextData "販売していない（isStatusOpen・itemStatus）" OutOfStock
            Just n | n > 0 -> gotRaw NextData ("在庫数 " <> show n) InStock
            Just _ -> gotRaw NextData "在庫数 0" OutOfStock
            Nothing -> missing NextData "在庫数（remainingItemCount）が無い"
        , seller = bodyField NextData "店名（shopSummaryInfo.shopName）が無い" shop
        , image = bodyField NextData "画像（productImageUrls）が無い" image `orField` d.image
        , shipping = missing Body "送料はJSで描画される（ブラウザで確かめる）"
        , buy = missing Body "カートのボタンはJSで描画される（ブラウザで確かめる）"
        , condition = if maybe False (T.isInfixOf "中古") title then withNote "商品名の表示から" (got NextData "中古") else d.condition
        , keys = d.keys {siteItemId = mfilter (not . T.null) (Just (digitsOnly (lastSegment ctx.pageUrl)))}
        }

-- BUYMA ----------------------------------------------------------------------

-- | 出品者はパーソナルショッパー（個人や店）。
--
-- 価格に「税込」の文字は無い。BUYMA の案内（/contents/duty/）では、「関税負担なし」の印が付いた商品は
-- 追加料金の負担がない（国内発送・関税元払い・発生時に返金）。印が無い海外発送の商品は、
-- 関税・輸入消費税が受け取りのときに別にかかることがある。そこで、印があるときだけ表示価格を支払う額として読む。
-- 「この商品は、売り切れました」は隠してあるひな形なので読まない。在庫は JSON-LD（色・サイズごと）で見る。
-- 検索で該当が無いと、HTTP 404 で「お探しの条件にあてはまる商品は見つかりませんでした」と出て、
-- その下に検索と関係のない商品が並ぶ。0件として読む。
buyma :: Adapter
buyma =
  Adapter
    { listX = (\def ctx ts html -> caveat (cards def defaultCards {priceClass = Just "Price_Txt", sellerClass = Just "product_Buyer", shippingClass = Just "fab-typo-price", emptyMarks = ["お探しの条件にあてはまる商品は見つかりませんでした"]} ctx ts html)) <$> lookupSite "buyma"
    , itemX = Just item
    }
 where
  caveat out =
    ListOut
      { offers = map (\o -> o {priceIncl = withNote "海外発送の商品は、関税・輸入消費税が別にかかることがある（個別ページの「関税負担なし」で確かめる）" o.priceIncl}) out.offers
      , zero = out.zero
      , follow = out.follow
      }
  item ctx d =
    let ts = ctx.tags
        ld = ldProduct ts
        priceWrap = fromMaybe [] (byId "priceWrap" ts)
        shown = parseYen =<< (visibleText <$> byId "abtest_display_pc" priceWrap)
        info = textOfClass "info_box_wrap" priceWrap
        noDuty = maybe False (textHas ["関税負担なし", "関税込"]) info
        inStock = any (\o -> (stockFromSchema =<< o.availability) == Just InStock) (variantOffers ld)
        stock = if inStock then withNote "色・サイズのどれかに在庫あり" (got JsonLd InStock) else d.stock
        title = mfilter (not . T.null) (visibleText <$> byId "item_h1" ts) <|> (T.strip . fst . T.breakOn "【BUYMA】" <$> ogTitle ts)
        cart = find (\t -> attr "id" t == Just "js-add-cart-action") ts
     in d
          { name = bodyField Body "商品名（#item_h1）が無い" title `orField` d.name
          , priceIncl = case (shown <|> d.priceIncl.value, noDuty) of
              (Just y, True) -> (gotRaw Body ("¥" <> show (unYen y)) y) {note = Just "「関税負担なし」の商品（追加料金の負担なし。BUYMA の案内による）。本文に「税込」の文字は無い"}
              (Just y, False) -> (missing Body "「関税負担なし」の表示が無い。関税・輸入消費税が別にかかることがある") {raw = Just (show (unYen y))}
              (Nothing, _) -> missing Body "価格（#priceWrap）が無い"
          , shipping = case info of
              Just t | Just s <- parseShipping t -> gotRaw Body t s
              Just t -> (missing Body "送料の表示を解釈できなかった") {raw = Just t}
              Nothing -> missing Body "送料の表示（info_box_wrap）が無い"
          , stock
          , seller = bodyField Body "出品者（js-itemdetail-shopper-name）の表示が無い" (textOfClass "js-itemdetail-shopper-name" ts)
          , buy = case (cart, stock.value) of
              (Just _, Just OutOfStock) -> got Body (BuyDisabled "カートに入れる")
              (Just _, _) -> got Body (BuyActive "カートに入れる")
              (Nothing, _) -> missing Body "カートのボタン（js-add-cart-action）が無い"
          , keys = d.keys {siteItemId = mfilter (not . T.null) (Just (digitsOnly (lastSegment ctx.pageUrl)))}
          }
  variantOffers :: Maybe LdProduct -> [LdOffer]
  variantOffers = maybe [] (.offers)

-- ロコンド -------------------------------------------------------------------

-- | 店は「ショップ：」の欄。名前・価格・在庫・画像は JSON-LD で、本文に「¥36 税込」とある。
-- 送料は商品ページに無い。検索はJSで描画されるので、一覧は読まない。
locondo :: Adapter
locondo = Adapter Nothing $ Just $ \ctx d ->
  let ts = ctx.tags
      shop = head (subtrees (hasClass "related_shopLink") ts) >>= firstTextWhere isA
      cart = find (hasClass "cartin") ts
   in d
        { priceIncl = confirmPrice (visibleText ts) d.priceIncl
        , seller = bodyField Body "ショップの表示（related_shopLink）が無い" shop `orField` d.seller
        , shipping = missing Body "個別ページに送料の表示なし（ご利用ガイドの「配送と送料」で確かめる）"
        , buy = case (cart, d.stock.value) of
            (Just _, Just OutOfStock) -> got Body (BuyDisabled "カートに入れる")
            (Just _, _) -> withNote "サイズごとの在庫は、ページの表で確かめる" (got Body (BuyActive "カートに入れる"))
            (Nothing, _) -> missing Body "カートのボタン（cartin）が無い"
        }
 where
  isA = \case
    TagOpen "a" _ -> True
    _ -> False
