-- | 直販の店のアダプタ（ロフト・Francfranc・コーナン・ディノス・ジャパネット・イオシス・カインズ）。
--
-- どの店も、名前・価格・在庫・画像は JSON-LD（汎用の抽出）から読む。ここに書くのは、
-- 店ごとの決まりだけ：税込の裏づけ、購入の入口、送料の表示、当てにならない表示。
module Meguri.Site.DirectStores
  ( loft
  , francfranc
  , kohnan
  , dinos
  , japanet
  , iosys
  , cainz
  , costco
  , ikea
  , sofmap
  ) where

import Data.Text qualified as T
import Meguri.Catalog (SiteDef, lookupSite)
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (atPath, ldValues, str)
import Meguri.Extract.Price (stockFromSchema)
import Meguri.Site.Common
import Meguri.Site.Store
import Meguri.Text (digitsOnly)
import Meguri.Types
import Protolude

-- | 一覧は、カード切り（"Meguri.Site.Store" の 'cards'）で読む
storeAdapter :: Text -> Text -> CardOpts -> (ItemCtx -> Text -> Draft -> Draft) -> Adapter
storeAdapter sid shop opts item =
  Adapter
    { listX = (\def ctx ts html -> cards def opts {sellerName = Just shop} ctx ts html) <$> site
    , itemX = Just (\ctx d -> item ctx (visibleText ctx.tags) (direct shop ctx d))
    }
 where
  site :: Maybe SiteDef
  site = lookupSite sid

-- | 直販の店に共通：販売者は店そのもの。JAN が型番の欄に入っていれば JAN として持つ
direct :: Text -> ItemCtx -> Draft -> Draft
direct shop _ d =
  d
    { seller = d.seller `orField` withNote "直販（販売者は店そのもの）" (got Body shop)
    , keys = d.keys {jan = d.keys.jan <|> (mfilter (\m -> T.length m == 13 && T.all (`elem` ("0123456789" :: [Char])) m) (digitsOnly <$> d.keys.model))}
    }

-- | 在庫が無ければ、購入の入口は無効として読む
buyUnless :: Draft -> Maybe BuyEntry -> Field Method BuyEntry
buyUnless d found = case (d.stock.value, found) of
  (Just OutOfStock, Just (BuyActive l)) -> withNote "在庫なしなので、押せないはず" (got Body (BuyDisabled l))
  (_, Just b) -> got Body b
  (_, Nothing) -> missing Body "カートのボタンが見つからない"

-- ロフト ---------------------------------------------------------------------

loft :: Adapter
loft = storeAdapter "loft" "ロフト" defaultCards $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , buy = buyUnless d (cartWhere "カートに入れる" (hasText "カートに入れる" ctx.tags) ctx.tags)
    , shipping = missing Body "個別ページに送料の表示なし（ご利用ガイドで確かめる）"
    }

-- Francfranc -----------------------------------------------------------------

francfranc :: Adapter
francfranc = storeAdapter "francfranc" "Francfranc" defaultCards $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , buy = buyUnless d (cartWhere "カートに入れる" (hasClass "js-add-to-cart") ctx.tags)
    , shipping =
        if "5,000円(税込)以上の場合、送料無料" `T.isInfixOf` compact body
          then freeOver Body (Yen 5000) "雑貨の商品金額合計が5,000円（税込）以上で送料無料" d.priceIncl.value
          else missing Body "送料の表示なし"
    }

-- コーナン -------------------------------------------------------------------

kohnan :: Adapter
kohnan = storeAdapter "kohnan" "コーナン" defaultCards {dropQuery = True} $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , buy = buyUnless d (cartWhere "カートに入れる" (maybe False ("/shop/cart/cart.aspx?goods=" `T.isPrefixOf`) . attr "href") ctx.tags)
    , shipping =
        withNote "店舗受取なら無料。メーカー直送・店舗受取のみの注意書きは、ページで読む" $
          if "7,700円(税込)以上ご購入" `T.isInfixOf` compact body
            then freeOver Body (Yen 7700) "7,700円（税込）以上で基本送料無料" d.priceIncl.value
            else missing Body "送料の表示なし"
    }

-- ディノス -------------------------------------------------------------------

-- | JSON-LD の在庫は offers.priceSpecification の中にある。
-- 本文の「在庫なし」は隠してあるひな形（display:none）なので、読まない。
dinos :: Adapter
dinos = storeAdapter "dinos" "ディノス" defaultCards {dropQuery = True, nameClass = Just "name"} $ \ctx body d ->
  let availability = head (mapMaybe (\v -> str =<< atPath ["offers", "priceSpecification", "availability"] v) (ldValues ctx.tags))
      stock = case availability of
        Just a | Just s <- stockFromSchema a -> gotRaw JsonLd a s
        _ -> d.stock
      d' = d {stock = stock}
   in d'
        { priceIncl = confirmPrice body d.priceIncl
        , buy = buyUnless d' (cartWhere "カートに入れる" (hasClass "box-cart-btn") ctx.tags)
        , shipping = case yenBefore "基本配送料:" body of
            Just fee -> withNote "会員のステージがゴールド以上なら無料" (gotRaw Body "基本配送料" (if fee == 0 then Free else Fixed fee))
            Nothing -> missing Body "基本配送料の表示なし"
        }

-- ジャパネットたかた -----------------------------------------------------------

-- | JSON-LD の価格は offers.Price（大文字の P）。送料は本文の「お支払総額（送料○円…）」。
-- 一覧には「下取りなし価格」と「下取り後価格」が並ぶ。売値の欄（product-price-cell）を読む。
japanet :: Adapter
japanet = storeAdapter "japanet" "ジャパネットたかた" defaultCards {priceClass = Just "product-price-cell"} $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , buy = buyUnless d (cartWhere "カートに入れる" (\t -> attr "alt" t == Just "カートに入れる") ctx.tags)
    , shipping = case yenBefore "(送料" body of
        Just fee -> withNote "下取品の回収料金などは別" (gotRaw Body "お支払総額（送料…）" (if fee == 0 then Free else Fixed fee))
        Nothing -> missing Body "「お支払総額（送料○円）」の表示なし"
    }

-- イオシス -------------------------------------------------------------------

iosys :: Adapter
iosys = storeAdapter "iosys" "イオシス" defaultCards $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , buy = buyUnless d (cartWhere "カートに入れる" (\t -> hasClass "btn-danger" t && hasText "カートに入れる" ctx.tags t) ctx.tags)
    , shipping = case yenBefore "基本送料は" body of
        Just fee -> withNote "パソコン・モニタが含まれると別途かかる" (gotRaw Body "基本送料" (Fixed fee))
        Nothing -> missing Body "送料の表示なし"
    , condition = bodyField Body "状態の表示なし" (rank body) `orField` d.condition
    }
 where
  -- 本文で最初に出てくる状態の表示
  rank body =
    let c = compact body
        found = [(T.length (fst (T.breakOn w c)), w) | w <- ["中古Sランク", "中古Aランク", "中古Bランク", "中古Cランク", "中古Dランク", "未使用品", "新品"], w `T.isInfixOf` c]
     in snd <$> head (sortOn fst found)

-- カインズ -------------------------------------------------------------------

-- | カートのボタンは、HTMLでは disabled（JS が受け取り方を決めてから有効にする）。
cainz :: Adapter
cainz = storeAdapter "cainz" "カインズ" defaultCards $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , buy = case cartWhere "カートに入れる" (hasText "カートに入れる" ctx.tags) (fromMaybe [] (head (subtrees (\t -> attr "id" t == Just "addToCartProductDetailPage") ctx.tags))) of
        Just b -> withNote "HTMLでは無効のことがある（受け取り方を選ぶと有効になる）。在庫と最大注文数は店舗で変わる" (got Body b)
        Nothing -> missing Body "カートのボタンが見つからない"
    , shipping =
        withNote "北海道・沖縄・離島は地域配送料が加わる。店舗受取は無料" $
          if "5,000円以上ご注文で基本送料無料" `T.isInfixOf` compact body
            then freeOver Body (Yen 5000) "5,000円以上で基本送料無料" d.priceIncl.value
            else missing Body "送料の表示なし"
    }

-- コストコ -------------------------------------------------------------------

-- | JSON-LD の @type は小文字の "product"。価格は税込で、送料も JSON-LD（shippingDetails）にある。
-- 本文に「税込」の文字は無いので、税込かどうかはサイトの決まりとして読む。
-- 「ログインして価格を表示」の商品は、価格が読めない。
costco :: Adapter
costco = storeAdapter "costco" "コストコ" defaultCards $ \ctx body d ->
  d
    { priceIncl = case d.priceIncl.value of
        Just _ -> d.priceIncl {note = Just "JSON-LD の価格。コストコのオンラインの価格は税込（2026-11-20 に確かめたサイトの決まり）"}
        Nothing -> missing JsonLd "価格が読めない（会員だけに見せる価格かもしれない）"
    , shipping =
        if "通常配送料込み" `T.isInfixOf` compact body
          then withNote "一部の地域は配送サーチャージが加わる" (gotRaw Body "通常配送料込み" Free)
          else d.shipping
    , buy = buyUnless d (cartWhere "カートに入れる" (hasText "カートに入れる" ctx.tags) ctx.tags)
    }

-- IKEA -----------------------------------------------------------------------

-- | 価格・在庫・画像は JSON-LD。カートのボタンはJSで描画され、送料と店舗の在庫は郵便番号を入れないと出ない。
ikea :: Adapter
ikea = storeAdapter "ikea" "IKEA" defaultCards $ \_ body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , shipping = missing Body "郵便番号を入れないと出ない（ブラウザで確かめる）"
    , buy = missing Body "カートのボタンはJSで描画される（ブラウザで確かめる）"
    }

-- ソフマップ -----------------------------------------------------------------

-- | 新品は JSON-LD に価格・在庫がある（ページは Shift_JIS）。中古は JSON-LD が無いので、ここでは読めない。
sofmap :: Adapter
sofmap = storeAdapter "sofmap" "ソフマップ" defaultCards $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , shipping = case yenBefore "送料" body of
        Just fee -> withNote "一部の離島・山間部などを除く" (gotRaw Body "送料" (if fee == 0 then Free else Fixed fee))
        Nothing -> missing Body "送料の表示なし"
    , buy = buyUnless d (cartWhere "カートに入れる" (\t -> attr "name" t == Just "cart_in_button") ctx.tags)
    }
