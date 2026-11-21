-- | 通常のブラウザで保存したHTMLを読むアダプタ
-- （ヨドバシ・ヤマダ・ケーズデンキ・ニトリ・Amazon・メルカリ・2nd STREET・じゃんぱら・eBay）。
--
-- どれも HTTP では読めないサイト。エージェントがブラウザで開いて保存したHTMLを、
-- @meguri search … --page <URL>=<file>@（1ページだけ読むなら @meguri item <URL> --html <file>@）で渡す。
-- 保存したHTMLには、おすすめや閲覧履歴の枠が混ざる。主の商品の欄だけを、id や目印で指して読む。
module Meguri.Site.Captured
  ( yodobashi
  , amazon
  , mercari
  , secondStreet
  , yamada
  , ksdenki
  , nitori
  , janpara
  , ebay
  ) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (LdProduct (..), ldProduct)
import Meguri.Extract.Price
import Meguri.Site.Common
import Meguri.Site.Store (compact, stockField, validEan13, visibleText, yenBefore)
import Meguri.Text (digitsOnly, queryParam, squish)
import Meguri.Types
import Protolude
import Text.HTML.TagSoup (Tag (..))

itemOnly :: (ItemCtx -> Draft -> Draft) -> Adapter
itemOnly f = Adapter Nothing (Just f)

-- | id で指した要素の部分木
byId :: Text -> Tags -> Maybe Tags
byId i ts = head (subtrees (\t -> attr "id" t == Just i) ts)

textById :: Text -> Tags -> Maybe Text
textById i ts = mfilter (not . T.null) (visibleText <$> byId i ts)

-- | 条件に合う要素と、その後ろの少しの文字（「価格」の隣の「（税込）」などを読む）
textAround :: (Tag Text -> Bool) -> Int -> Tags -> Maybe Text
textAround p n ts = case dropWhile (not . p) ts of
  [] -> Nothing
  rest -> Just (squish (T.unwords [x | TagText x <- take n rest]))

byClass :: Text -> Tags -> Maybe Tags
byClass c ts = head (subtrees (hasClass c) ts)

textByClass :: Text -> Tags -> Maybe Text
textByClass c ts = mfilter (not . T.null) (visibleText <$> byClass c ts)

-- ヨドバシ.com ----------------------------------------------------------------

-- | 主の商品の値は、決まった id の要素にある。おすすめの枠（js_pp… など）は読まない。
yodobashi :: Adapter
yodobashi = itemOnly $ \ctx d ->
  let ts = ctx.tags
      -- 価格の要素の直後に「（税込）」が続く
      priceCell = fromMaybe [] (head (subtrees (\t -> attr "id" t == Just "js_scl_unitPrice") ts))
      afterPrice = T.concat [x | TagText x <- take 8 (drop (length priceCell) (dropWhile (\t -> attr "id" t /= Just "js_scl_unitPrice") ts))]
      price = parseYen =<< textById "js_scl_unitPrice" ts
      stock = stockField "在庫の表示（salesInfoTxt）が無い" (textById "salesInfoTxt" ts)
      cart = find (\t -> attr "id" t == Just "js_m_submitRelated") ts
   in d
        { name = bodyField Body "商品名（products_maintitle）が無い" (textById "products_maintitle" ts) `orField` d.name
        , priceIncl = case (price, taxMark afterPrice) of
            (Just y, Just MarkIncl) -> gotRaw Body (toS (show (unYen y) :: [Char]) <> "（税込）") y
            (Just _, _) -> missing Body "価格の横に「税込」の表示が無い"
            _ -> missing Body "価格（js_scl_unitPrice）が無い"
        , shipping = case textById "js_deliveryPriceTD" ts of
            Just t | "配達料金無料" `T.isInfixOf` t -> gotRaw Body t Free
            Just t | Just s <- parseShipping t -> gotRaw Body t s
            Just t -> (missing Body "配達料の表示を解釈できなかった") {raw = Just t}
            Nothing -> missing Body "配達料の表示（js_deliveryPriceTD）が無い"
        , stock
        , seller = withNote "直販（販売者は店そのもの）" (got Body "ヨドバシ.com")
        , buy = case (cart, stock.value) of
            (Just _, Just OutOfStock) -> got Body (BuyDisabled "ショッピングカートに入れる")
            (Just _, _) -> got Body (BuyActive "ショッピングカートに入れる")
            (Nothing, _) -> missing Body "カートのボタン（js_m_submitRelated）が無い"
        , points =
            withNote "ログインなしの表示（参考）" $
              bodyField Body "ポイントの表示なし" (readMaybe . T.filter (/= ',') . T.takeWhile (\c -> c `elem` ("0123456789," :: [Char])) =<< textById "js_pointTD" ts)
        , keys = d.keys {siteItemId = mfilter (not . T.null) (Just (digitsOnly (T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') (stripQuery ctx.pageUrl)))))}
        }

-- Amazon.co.jp ---------------------------------------------------------------

-- | 価格は「支払う価格」（priceToPay）の欄。隣の参考価格（定価）は読まない。
amazon :: Adapter
amazon = itemOnly $ \ctx d ->
  let ts = ctx.tags
      priceBox = byId "corePriceDisplay_desktop_feature_div" ts <|> byId "corePrice_feature_div" ts
      toPay = priceBox >>= \b -> head (subtrees (hasClass "priceToPay") b)
      price = parseYen =<< (toPay >>= textByClass "a-price-whole")
      taxIncl = maybe False (T.isInfixOf "税込" . visibleText) priceBox
      stock = stockField "在庫の表示（availability）が無い" (textById "availability" ts)
      sellerName =
        (T.strip . T.takeWhile (/= '（') <$> textById "sellerProfileTriggerId" ts)
          <|> (if maybe False (T.isInfixOf "Amazon.co.jp" . visibleText) (byId "merchantInfoFeature_feature_div" ts) then Just "Amazon.co.jp" else Nothing)
      delivery = textById "deliveryBlockMessage" ts <|> textById "mir-layout-DELIVERY_BLOCK" ts
      image = do
        img <- find (\t -> attr "id" t == Just "landingImage") ts
        attr "data-old-hires" img <|> attr "src" img
      asin = T.take 10 (T.drop 4 (snd (T.breakOn "/dp/" ctx.pageUrl)))
   in d
        { name = bodyField Body "商品名（productTitle）が無い" (textById "productTitle" ts) `orField` d.name
        , priceIncl = case price of
            Just y | taxIncl -> gotRaw Body "支払う価格（税込）" y
            Just _ -> missing Body "価格の欄に「税込」の表示が無い"
            Nothing -> missing Body "支払う価格（priceToPay）が無い。出品者別の一覧（?aod=1）を見る"
        , shipping = case delivery of
            Just t
              | "無料配送" `T.isPrefixOf` T.strip t || "配送料無料" `T.isInfixOf` t -> gotRaw Body (T.take 40 t) Free
              | Just s <- parseShipping t -> gotRaw Body (T.take 40 t) s
              | otherwise -> (missing Body "配送の欄を解釈できなかった") {raw = Just (T.take 60 t)}
            Nothing -> missing Body "配送の欄（deliveryBlockMessage）が無い"
        , stock
        , seller = bodyField Body "販売元の表示が無い" (mfilter (not . T.null) sellerName)
        , image = bodyField Body "画像（landingImage）が無い" image `orField` d.image
        , buy = case find (\t -> attr "id" t == Just "add-to-cart-button") ts of
            Just _ -> got Body (BuyActive "カートに入れる")
            Nothing -> missing Body "カートのボタン（add-to-cart-button）が無い"
        , keys = d.keys {siteItemId = if T.length asin == 10 then Just asin else d.keys.siteItemId}
        }

-- メルカリ -------------------------------------------------------------------

byTestId :: Text -> Tags -> Maybe Tags
byTestId i ts = head (subtrees (\t -> attr "data-testid" t == Just i) ts)

-- | 描画後の JSON-LD に名前・価格・在庫が入る。送料・状態・購入の入口は、data-testid の欄から読む。
mercari :: Adapter
mercari = itemOnly $ \ctx d ->
  let ts = ctx.tags
      priceText = textAround (\t -> attr "data-testid" t == Just "price") 14 ts
      button = visibleText <$> byTestId "checkout-button" ts
      sold = maybe False (T.isInfixOf "売り切れ") button
      shippingText = (\x -> T.unwords (take 1 (T.words x))) . visibleText <$> byTestId "配送料の負担" ts
      sellerLabel = attr "aria-label" =<< find (\t -> attr "data-location" t == Just "item_details:seller_info") ts
   in d
        { priceIncl = case (d.priceIncl.value, priceText) of
            (Just y, Just t) | "税込" `T.isInfixOf` t -> (got JsonLd y) {note = Just "本文の価格の欄にも「税込」とある"}
            (Just _, _) -> missing JsonLd "価格の欄に「税込」の表示が無い"
            _ -> d.priceIncl
        , shipping = case shippingText of
            Just t | "送料込み" `T.isInfixOf` t -> gotRaw Body t Free
            Just t | "着払い" `T.isInfixOf` t -> gotRaw Body t BuyerPays
            Just t -> (missing Body "配送料の負担を解釈できなかった") {raw = Just t}
            Nothing -> missing Body "配送料の負担の欄が無い"
        , stock = case (button, d.stock.value) of
            (Just _, _) | sold -> gotRaw Body "売り切れ" Sold
            (Just b, _) -> gotRaw Body b Selling
            (Nothing, Just OutOfStock) -> got JsonLd Sold
            _ -> missing Body "購入のボタンが無い"
        , buy = case button of
            Just b | sold -> got Body (BuyDisabled (squish b))
            Just b -> got Body (BuyActive (squish b))
            Nothing -> missing Body "購入のボタン（checkout-button）が無い"
        , condition = bodyField Body "商品の状態の欄が無い" ((\x -> T.unwords (take 1 (T.words x))) . visibleText <$> byTestId "商品の状態" ts)
        , seller = bodyField Body "出品者の表示が無い" (mfilter (not . T.null) (T.strip . T.takeWhile (/= ',') <$> sellerLabel))
        , keys = d.keys {siteItemId = Just (T.takeWhileEnd (/= '/') (stripQuery ctx.pageUrl))}
        }

-- 2nd STREET -----------------------------------------------------------------

-- | 「店舗: セカンドストリート 尼崎杭瀬店 お問い合わせ番号: …」から、店舗の名前だけ
shopName :: Text -> Text
shopName t =
  let afterLabel = T.drop 1 (T.dropWhile (\c -> c /= ':' && c /= '：') t)
   in T.strip (fst (T.breakOn "お問い合わせ番号" afterLabel))

-- | 主の商品は #goodsDescription の中。おすすめの枠（itemCard）にも同じ形の値が並ぶので、読まない。
-- 13桁の商品番号は、楽天店・Yahoo!店と共通。
secondStreet :: Adapter
secondStreet = itemOnly $ \ctx d ->
  let main = fromMaybe [] (byId "goodsDescription" ctx.tags)
      ld = ldProduct ctx.tags
      title = T.unwords (catMaybes [textByClass "blandName" main, textByClass "productName" main])
      priceBox = byId "price" ctx.tags
      price = parseYen =<< (priceBox >>= textByClass "priceNum")
      -- 「￥16,390 税込 送料：¥770 税込」の、送料より前に「税込」があるか
      taxIncl = maybe False (T.isInfixOf "税込" . fst . T.breakOn "送料" . visibleText) priceBox
      shipText = textByClass "shipping_price" main
      stock = case d.stock.value of
        Just InStock -> d.stock {value = Just Selling}
        _ -> d.stock
   in d
        { name = bodyField Body "商品名が無い" (mfilter (not . T.null) (Just title)) `orField` d.name
        , priceIncl = case price of
            Just y | taxIncl -> gotRaw Body "価格（税込）" y
            Just _ -> missing Body "価格の横に「税込」の表示が無い"
            Nothing -> missing Body "価格（#price .priceNum）が無い"
        , shipping = case shipText of
            Just t | Just s <- parseShipping (compact t) -> withNote "「個別送料」の商品は別にかかる" (gotRaw Body (T.take 30 t) s)
            Just t -> (missing Body "送料の表示を解釈できなかった") {raw = Just (T.take 40 t)}
            Nothing -> missing Body "送料の表示が無い"
        , stock
        , condition = bodyField Body "状態の表示が無い" (T.strip . T.takeWhileEnd (\c -> c /= ':' && c /= '：') <$> textByClass "conditionStatusTag" main)
        , seller = bodyField Body "店舗の表示が無い" (mfilter (not . T.null) (shopName <$> textByClass "shopInfomation" ctx.tags))
        , buy = case (find (hasClass "addCartWideBtn") main, stock.value) of
            (Just _, Just Selling) -> got Body (BuyActive "カートに入れる")
            (Just _, _) -> got Body (BuyDisabled "カートに入れる")
            (Nothing, _) -> missing Body "カートのボタンが無い"
        , keys =
            d.keys
              { sharedStockId = thirteenDigits ctx.pageUrl <|> (ld >>= (.sku))
              , siteItemId = thirteenDigits ctx.pageUrl <|> d.keys.siteItemId
              , model = T.strip <$> (ld >>= (.mpn))
              }
        }

-- ヤマダウェブコム -------------------------------------------------------------

-- | 主の商品の欄は l-list-product-info-main。関連商品の欄にも同じ形の価格が並ぶので、読まない。
--
-- 主の商品の価格の欄に「税込」の文字は無い（関連商品の欄には「¥…(税込)」と出る）。
-- ヤマダウェブコムの価格は税込として読む（2026-11-21 に確かめたサイトの決まり）。
-- 在庫は、配送先のエリアごとの表示。お届けの目安が出ていれば在庫あり、無ければ「販売終了しました」と出る。
yamada :: Adapter
yamada = itemOnly $ \ctx d ->
  let ts = ctx.tags
      main = fromMaybe [] (byClass "l-list-product-info-main" ts)
      price = parseYen =<< textByClass "price-large" main
      labels = map visibleText (subtrees (hasClass "tags") main)
      delivery = textByClass "list-product-info__delivery-date" main
      cart = find (hasClass "main_cart_btn") ts
      stock = case delivery of
        Just t | "お届け" `T.isInfixOf` t -> withNote "配送先のエリアで変わる（保存したときのエリアの表示）" (gotRaw Body (T.take 40 t) InStock)
        _ | "販売終了" `T.isInfixOf` visibleText main -> gotRaw Body "販売終了しました" OutOfStock
        _ -> missing Body "お届けの目安（list-product-info__delivery-date）が無い"
   in d
        { name = bodyField Body "商品名（product-info__title）が無い" (textByClass "product-info__title" ts) `orField` d.name
        , priceIncl = case price of
            Just y -> (gotRaw Body ("¥" <> show (unYen y)) y) {note = Just "価格の欄に「税込」の文字は無い。ヤマダウェブコムの価格は税込（2026-11-21 に確かめたサイトの決まり）"}
            Nothing -> missing Body "価格（price-large）が無い"
        , shipping =
            if "送料無料" `elem` labels
              then gotRaw Body "送料無料" Free
              else missing Body "「送料無料」の札が無い（額は「配送料金について」のページで確かめる）"
        , stock
        , seller = withNote "直販（販売者は店そのもの）" (got Body "ヤマダウェブコム")
        , buy = case (cart, stock.value) of
            (Just _, Just OutOfStock) -> got Body (BuyDisabled "カートに入れる")
            (Just _, _) -> got Body (BuyActive "カートに入れる")
            (Nothing, _) -> missing Body "カートのボタン（main_cart_btn）が無い"
        , keys = d.keys {siteItemId = mfilter (not . T.null) (Just (digitsOnly (T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') (stripQuery ctx.pageUrl)))))}
        }

-- ケーズデンキ ---------------------------------------------------------------

-- | 価格は itemPriceIncld（税込）と itemPriceBase（税別）の2つが並ぶ。在庫は pdStock、送料は icoPostage の札。
-- 名前・型番・JAN（sku）は JSON-LD にある。
ksdenki :: Adapter
ksdenki = itemOnly $ \ctx d ->
  let ts = ctx.tags
      incl = byClass "itemPriceIncld" ts
      price = parseYen =<< (incl >>= textByClass "itemPriceValue")
      memo = incl >>= textByClass "itemPriceMemo"
      stock = stockField "在庫の表示（pdStock）が無い" (textByClass "pdStock" ts)
      postage = attr "aria-label" =<< find (hasClass "icoPostage") ts
      cart = find (hasClass "cmnBtnAddCart") ts
   in d
        { priceIncl = case (price, taxMark =<< memo) of
            (Just y, Just MarkIncl) -> gotRaw Body (show (unYen y) <> "円(税込)") y
            (Just _, _) -> missing Body "価格の横に「税込」の表示が無い"
            _ -> missing Body "価格（itemPriceIncld）が無い"
        , priceExcl = bodyField Body "税別の価格の表示なし" (parseYen =<< (byClass "itemPriceBase" ts >>= textByClass "itemPriceValue"))
        , shipping = case postage of
            Just t | Just s <- parseShipping t -> gotRaw Body t s
            Just t -> (missing Body "送料の札を解釈できなかった") {raw = Just t}
            Nothing -> missing Body "送料の札（icoPostage）が無い"
        , stock
        , seller = withNote "直販（販売者は店そのもの）" (got Body "ケーズデンキ")
        , buy = case (cart, stock.value) of
            (Just _, Just OutOfStock) -> got Body (BuyDisabled "カートに入れる")
            (Just _, _) -> got Body (BuyActive "カートに入れる")
            (Nothing, _) -> missing Body "カートのボタン（cmnBtnAddCart）が無い"
        , keys = d.keys {jan = d.keys.jan <|> mfilter validEan13 (d.keys.siteItemId <|> queryParam "sku" ctx.pageUrl)}
        }

-- ニトリネット ---------------------------------------------------------------

-- | 名前・在庫・画像は JSON-LD（色ごとの Product）。価格は cart-price の欄。
--
-- 価格の欄に「税込」の文字は無い（比べてある以前の価格には「(税込)」と出る）。
-- ニトリネットの価格は税込として読む（2026-11-21 に確かめたサイトの決まり）。
-- 送料は仕様の表の「送料」の行。「個別送料」の商品は、合計が11,000円未満だと基本送料も加わる。
-- 受取店舗の在庫とお届け先の地名は読まない。レビューの欄も読まない。
nitori :: Adapter
nitori = itemOnly $ \ctx d ->
  let ts = ctx.tags
      -- 価格の欄は2か所（上の要約は left-cart-price、カートの欄は cart-price）
      price = parseYen =<< (textByClass "cart-price" ts <|> textByClass "left-cart-price" ts)
      rows = subtrees isTr (fromMaybe [] (byId "product-specification-table" ts))
      shipText = textByClass "rightcolumn" =<< find (\r -> textByClass "title-S" r == Just "送料") rows
      cart = find (\t -> attr "id" t == Just "p-addItem") ts
   in d
        { priceIncl = case price of
            Just y -> (gotRaw Body (show (unYen y) <> "円") y) {note = Just "価格の欄に「税込」の文字は無い。ニトリネットの価格は税込（2026-11-21 に確かめたサイトの決まり）"}
            Nothing -> missing Body "価格（cart-price）が無い"
        , shipping = case shipText of
            Just t
              | Just fee <- yenBefore "個別送料:1点につき" t ->
                  if maybe False (>= Yen 11000) price && "11,000円以上で基本送料無料" `T.isInfixOf` compact t
                    then withNote "個別送料（1点ごと）。基本送料は、合計が11,000円以上なので無料" (gotRaw Body (T.take 60 t) (Fixed fee))
                    else gotRaw Body (T.take 60 t) (ShipUnknown ("個別送料 " <> show (unYen fee) <> "円（1点ごと）に、基本送料が加わる。合計11,000円以上なら基本送料は無料"))
              | Just s <- parseShipping t -> gotRaw Body (T.take 60 t) s
              | otherwise -> (missing Body "送料の行を解釈できなかった") {raw = Just (T.take 60 t)}
            Nothing -> missing Body "仕様の表に「送料」の行が無い"
        , stock = withNote "ネットの在庫（JSON-LD）。受取店舗の在庫は別" d.stock
        , seller = withNote "直販（販売者は店そのもの）" (got Body "ニトリ")
        , buy = case (cart, d.stock.value) of
            (Just _, Just OutOfStock) -> got Body (BuyDisabled "カートに入れる")
            (Just _, _) -> withNote "注意事項のある商品は、同意するまで押せない" (got Body (BuyActive "カートに入れる"))
            (Nothing, _) -> missing Body "カートのボタン（p-addItem）が無い"
        , keys = d.keys {siteItemId = mfilter (not . T.null) (Just (T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') (stripQuery ctx.pageUrl))))}
        }
 where
  isTr = \case
    TagOpen "tr" _ -> True
    _ -> False

-- じゃんぱら -----------------------------------------------------------------

-- | 在庫は itemdetail_table の行ごと（店舗・状態・価格）。行が複数あれば、いちばん安い行を読む。
-- 表の見出しが「価格（税込）」。送料は画像（送料メーター）で出るので、読めない。
-- og:image は分類の画像なので使わない。
janpara :: Adapter
janpara = itemOnly $ \ctx d ->
  let ts = ctx.tags
      table = fromMaybe [] (byClass "itemdetail_table" ts)
      rowPrice r = parseYen =<< textByClass "detail_item_money" r
      rows = sortOn rowPrice (filter (isJust . rowPrice) (subtrees isTr table))
      best = head rows
      taxIncl = "価格(税込)" `T.isInfixOf` compact (visibleText table)
      icons r = [a | t <- r, hasClass "kbn" t, Just a <- [attr "alt" t]]
      shop r = byClass "itemdata_shop" r >>= \box -> firstTextWhere isA box
      comment r = squish <$> textByClass "itemdetail_comment" r
      photo = find (\u -> not ("/images/sale/category_" `T.isInfixOf` u)) (mapMaybe (attr "src") (filter (hasClass "item_img") ts))
      title = T.strip . fst . T.breakOn "の詳細｜" <$> titleText ts
      cart = best >>= find (hasClass "cart_btn")
      more = if length rows > 1 then withNote ("在庫 " <> show (length rows) <> " 点のうち、いちばん安いもの") else identity
   in d
        { name = bodyField Body "商品名（title）が無い" (mfilter (not . T.null) title)
        , priceIncl = case best >>= rowPrice of
            Just y | taxIncl -> more (gotRaw Body "価格（税込）" y)
            Just _ -> missing Body "表の見出しに「税込」の表示が無い"
            Nothing -> missing Body "在庫の行（itemdetail_table）が無い"
        , shipping = missing Body "送料は画像（送料メーター）で出る。ご利用案内で確かめる"
        , stock = case best of
            Just _ -> gotRaw Body "在庫の行あり" Selling
            Nothing -> missing Body "在庫の行が無い（売り切れかもしれない）"
        , condition = case best of
            Just r -> bodyField Body "状態の表示なし" (mfilter (not . T.null) (Just (T.intercalate "・" (icons r) <> maybe "" (" ／ " <>) (comment r))))
            Nothing -> missing Body "在庫の行が無い"
        , seller = bodyField Body "店舗の表示が無い" (("じゃんぱら " <>) <$> (best >>= shop))
        , image = bodyField Body "商品の写真が無い（分類の画像だけ）" photo
        , buy = case cart of
            Just _ -> got Body (BuyActive "カートに入れる")
            Nothing -> missing Body "カートのボタン（cart_btn）が無い"
        , keys = d.keys {siteItemId = queryParam "SRCODE" ctx.pageUrl <|> queryParam "ITMCODE" ctx.pageUrl}
        }
 where
  isTr = \case
    TagOpen "tr" _ -> True
    _ -> False
  isA = \case
    TagOpen "a" _ -> True
    _ -> False

-- eBay -----------------------------------------------------------------------

-- | 海外の出品。価格は外貨で、円の額は目安の換算（「約 5,223 円」）。関税・輸入消費税・手数料は別にかかる。
-- 税込価格は出さず、換算の額を税抜の欄に入れる（支払額は出ない）。日本語の表示のページで確かめた。
ebay :: Adapter
ebay = itemOnly $ \ctx d ->
  let ts = ctx.tags
      byTest i = firstTextWhere (\t -> attr "data-testid" t == Just i) ts
      primary = byTest "x-price-primary"
      approx = parseYen =<< byTest "x-price-approx"
      inYen t = textHas ["円", "JPY"] t
      shipText = textByClass "ux-labels-values--shipping" ts
      importText = textByClass "ux-labels-values--importCharges" ts
      qty = textById "qtyAvailability" ts
      cond = byClass "x-item-condition-text" ts >>= firstTextWhere (hasClass "ux-textspans")
      sellerName = byClass "x-sellercard-atf__about-seller" ts >>= firstTextWhere (hasClass "ux-textspans")
      button i label = label <$ find (\t -> attr "id" t == Just i) ts
      ended = textHas ["この出品は終了", "This listing has ended", "This listing was ended"] (visibleText ts)
   in d
        { priceIncl = (missing Body "海外の出品。関税・輸入消費税・手数料が別にかかる（税込の額は出ない）") {raw = primary}
        , priceExcl = case (approx, primary) of
            (Just y, _) -> withNote "円の額は目安の換算。関税・輸入消費税・手数料は含まない" (gotRaw Body (fromMaybe "" primary) y)
            (Nothing, Just p) | inYen p, Just y <- parseYen p -> withNote "関税・輸入消費税・手数料は含まない" (gotRaw Body p y)
            (Nothing, Just p) -> (missing Body "外貨の価格だけで、円の換算が無い") {raw = Just p}
            _ -> missing Body "価格（x-price-primary）が無い"
        , shipping = case shipText of
            Just t
              | textHas ["送料無料", "無料", "Free"] t -> gotRaw Body (T.take 60 t) Free
              | Just y <- parseYen (snd (T.breakOn "JPY" t)) ->
                  withNote ("円の額は目安の換算" <> maybe "" ("。" <>) (T.take 50 <$> importText)) (gotRaw Body (T.take 60 t) (Fixed y))
              | otherwise -> (missing Body "送料を円で読めなかった") {raw = Just (T.take 60 t)}
            Nothing -> missing Body "送料の欄（ux-labels-values--shipping）が無い"
        , stock = case qty of
            _ | ended -> gotRaw Body "出品は終了" Sold
            Just t | textHas ["在庫", "available", "残り"] t -> gotRaw Body (T.take 30 t) InStock
            _ -> d.stock
        , condition = bodyField Body "状態の表示が無い" cond `orField` d.condition
        , seller = bodyField Body "出品者の表示（x-sellercard-atf）が無い" sellerName
        , buy = case button "binBtn_btn_1" "今すぐ買う" <|> button "atcBtn_btn_1" "カートに追加" of
            Just l | ended -> got Body (BuyDisabled l)
            Just l -> got Body (BuyActive l)
            Nothing -> missing Body "購入のボタン（binBtn_btn_1）が無い"
        , keys =
            d.keys
              { siteItemId = mfilter (not . T.null) (Just (T.takeWhile isDigit (T.drop 5 (snd (T.breakOn "/itm/" ctx.pageUrl)))))
              , -- 出品者が書かなかった欄は「Does not apply」になる
                model = mfilter (\m -> T.toLower m /= "does not apply") d.keys.model
              }
        }
