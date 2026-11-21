-- | 直販の店のアダプタ（ロフト・ハンズ・Francfranc・コーナン・ディノス・ジャパネット・イオシス・イシバシ楽器・カインズなど）。
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
  , hands
  , ishibashi
  , cado
  , okoku
  , akachan
  ) where

import Data.Text qualified as T
import Meguri.Catalog (Channel, lookupSite)
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (atPath, ldValues, str)
import Meguri.Extract.Price (TaxMark (..), parsePrice, parseYen, stockFromSchema)
import Meguri.Site.Common
import Meguri.Site.Store
import Meguri.Text (digitsOnly)
import Meguri.Types
import Protolude
import Text.HTML.TagSoup (Tag (..))

-- | 一覧は、カード切り（"Meguri.Site.Store" の 'cards'）で読む
storeAdapter :: Text -> Text -> CardOpts -> (ItemCtx -> Text -> Draft -> Draft) -> Adapter
storeAdapter sid shop opts item =
  Adapter
    { listX = (\def ctx ts html -> cards def opts {sellerName = Just shop} ctx ts html) <$> site
    , itemX = Just (\ctx d -> item ctx (visibleText ctx.tags) (direct shop ctx d))
    }
 where
  site :: Maybe Channel
  site = lookupSite sid

-- | 一覧を読まない店（検索結果がJSで描画される）
itemOnly :: Adapter -> Adapter
itemOnly a = Adapter {listX = Nothing, itemX = a.itemX}

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

-- | 検索結果はJSで描画される。HTMLに並ぶ商品は Pickup・Ranking などの枠で、検索結果ではないので、一覧は読まない。
francfranc :: Adapter
francfranc = itemOnly $ storeAdapter "francfranc" "Francfranc" defaultCards $ \ctx body d ->
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
-- 検索結果は #listPreview の中だけ。その後ろに、検索と関係のない商品の枠が並ぶ。
dinos :: Adapter
dinos = storeAdapter "dinos" "ディノス" defaultCards {dropQuery = True, nameClass = Just "name", regionId = Just "listPreview"} $ \ctx body d ->
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

-- ハンズ ---------------------------------------------------------------------

-- | JSON-LD に Product は無い。値は #item-info の欄から読む。
-- 値下げ品は価格が2つ出る。いまの価格は normal の欄（検索結果に出る額と同じ）で、discount の欄は読まない。
-- 商品番号は JAN。
-- 検索結果は #search-result-list の中だけ。その後ろに「今、コレ売れました」の枠（検索と関係のない商品）が並ぶ。
hands :: Adapter
hands = storeAdapter "hands" "ハンズ" defaultCards {priceClass = Just "price", regionId = Just "search-result-list"} $ \ctx body d ->
  let info = fromMaybe [] (head (subtrees (\t -> attr "id" t == Just "item-info") ctx.tags))
      priceBox = fromMaybe [] (head (subtrees (hasClass "price") info))
      now = fromMaybe priceBox (head (subtrees (hasClass "normal") priceBox))
      incl = textOfClass "include" now
      other = textOfClass "include" =<< head (subtrees (hasClass "discount") priceBox)
      price = case parsePrice =<< incl of
        Just (y, Just MarkIncl) ->
          maybe identity (\o -> withNote ("価格が2つ出ている。いまの価格の欄（normal）を読んだ。もう1つは " <> o)) other (gotRaw Body (fromMaybe "" incl) y)
        Just _ -> missing Body "価格の欄に「税込」の表示が無い"
        Nothing -> missing Body "価格（#item-info .price）が無い"
      d' = d {stock = stockField "在庫の表示（stockStatus）が無い" (textOfClass "stockStatus" info)}
      code = T.takeWhileEnd isDigit (T.dropWhileEnd (== '/') (stripQuery ctx.pageUrl))
   in d'
        { name = bodyField Body "商品名（h1.title）が無い" (firstTextWhere (\t -> isH1 t && hasClass "title" t) ctx.tags) `orField` d.name
        , priceIncl = price
        , priceExcl = bodyField Body "本体価格の表示なし" (parseYen =<< textOfClass "exclude" now)
        , buy = buyUnless d' (cartWhere "カートに入れる" (\t -> hasClass "modButton" t && hasClass "order" t) info)
        , shipping =
            if "5,000円(税込)以上で送料無料" `T.isInfixOf` compact body
              then freeOver Body (Yen 5000) "ネットストアは合計5,000円（税込）以上で送料無料（一部商品を除く）" price.value
              else missing Body "送料の表示なし"
        , keys = d.keys {jan = d.keys.jan <|> mfilter validEan13 (Just code), siteItemId = d.keys.siteItemId <|> mfilter (not . T.null) (Just code)}
        }
 where
  isH1 = \case
    TagOpen "h1" _ -> True
    _ -> False

-- イシバシ楽器 ---------------------------------------------------------------

-- | MakeShop。名前・価格・在庫・画像は JSON-LD。本文に「¥ 110 税込」「配送料: ¥ 990」があり、
-- 店舗とランクは im-product-rank の data-name（「横浜店 S：新品」）にある。
-- 商品コードは「09-{JAN}」の形のことがある。売れた後も掲載が残ることがある。
ishibashi :: Adapter
ishibashi = storeAdapter "ishibashi" "イシバシ楽器" defaultCards {priceClass = Just "im-product-price", zeroMarks = ["result-info-hitnum\">0<"]} $ \ctx body d ->
  let rank = T.strip <$> (attr "data-name" =<< find (hasClass "im-product-rank") ctx.tags)
      code = T.takeWhileEnd isDigit <$> d.keys.siteItemId
   in d
        { priceIncl = confirmPrice body d.priceIncl
        , shipping = case yenBefore "配送料:" body of
            Just fee -> gotRaw Body "配送料" (if fee == 0 then Free else Fixed fee)
            Nothing -> missing Body "配送料の表示なし"
        , buy = buyUnless d (cartWhere "カートに入れる" (hasClass "cart-button") ctx.tags)
        , condition = bodyField Body "店舗とランクの表示（im-product-rank）なし" (mfilter (not . T.null) rank) `orField` d.condition
        , keys = d.keys {jan = d.keys.jan <|> mfilter validEan13 code}
        }

-- cado -----------------------------------------------------------------------

-- | Shopify。色ごとの offers が JSON-LD にある。本文の「売り切れ」は値札のひな形で、
-- 在庫ありの商品にも入っているので読まない（在庫は JSON-LD）。
cado :: Adapter
cado = storeAdapter "cado" "cado" defaultCards {dropQuery = True, soldOutMarks = []} $ \ctx body d ->
  let c = compact body
   in d
        { priceIncl = confirmPrice body d.priceIncl
        , buy = buyUnless d (cartWhere "カートに追加する" (hasClass "product-form__cart-submit") ctx.tags)
        , shipping =
            if "4,000円(税込)以上のご購入で送料無料" `T.isInfixOf` c && "送料は一律550円" `T.isInfixOf` c
              then gotRaw Body "4,000円（税込）以上で送料無料。未満は一律550円（税込）" (FreeOver (Yen 4000) (Yen 550))
              else missing Body "送料の表示なし"
        }

-- 買取王国 -------------------------------------------------------------------

-- | Shopify。価格・在庫・画像は JSON-LD。ランクは本文（c_main-product__rank-line）。
-- 送料は商品ページに無い。
okoku :: Adapter
okoku = storeAdapter "okoku" "買取王国" defaultCards {dropQuery = True} $ \ctx body d ->
  d
    { priceIncl = confirmPrice body d.priceIncl
    , buy = buyUnless d (cartWhere "カートに追加" (\t -> attr "id" t == Just "AddToCart") ctx.tags)
    , shipping = missing Body "個別ページに送料の表示なし（ご利用ガイドで確かめる）"
    , condition = bodyField Body "ランクの表示なし" (textOfClass "c_main-product__rank-line" ctx.tags) `orField` d.condition
    }

-- 赤ちゃん本舗 ---------------------------------------------------------------

-- | 価格・在庫は JSON-LD。送料は本文の「一般商品 600円（税込）…5,500円（税込）以上で送料当社負担」。
-- 店舗ごとの在庫の一覧は読まない（ネットの在庫ではない）。
akachan :: Adapter
akachan = storeAdapter "akachan" "赤ちゃん本舗" defaultCards $ \ctx body d ->
  let c = compact body
      code = T.take 13 (T.drop (T.length ("JANコード:" :: Text)) (snd (T.breakOn "JANコード:" c)))
   in d
        { priceIncl = confirmPrice body d.priceIncl
        , buy = buyUnless d (cartWhere "買い物かごに入れる" (\t -> attr "value" t == Just "買い物かごに入れる") ctx.tags)
        , shipping = case yenBefore "一般商品" body of
            Just fee
              | "5,500円(税込)以上で送料当社負担" `T.isInfixOf` c ->
                  withNote "お届け先1件につき。ギフト商品は別の額。一部商品・地域を除く" (gotRaw Body "一般商品の送料。5,500円（税込）以上で送料当社負担" (FreeOver (Yen 5500) fee))
            _ -> missing Body "送料の表示なし"
        , -- JSON-LD の mpn は、型番ではなく店の商品番号
          keys = d.keys {jan = d.keys.jan <|> mfilter validEan13 (Just code), siteItemId = d.keys.siteItemId <|> d.keys.model, model = Nothing}
        }
