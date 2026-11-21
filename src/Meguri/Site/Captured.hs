-- | 通常のブラウザで保存したHTMLを読むアダプタ（ヨドバシ・Amazon・メルカリ・2nd STREET）。
--
-- どれも HTTP では読めないサイト。エージェントがブラウザで開いて保存したHTMLを、
-- @meguri item <URL> --html <file>@ で渡す。
-- 保存したHTMLには、おすすめや閲覧履歴の枠が混ざる。主の商品の欄だけを、id や目印で指して読む。
module Meguri.Site.Captured
  ( yodobashi
  , amazon
  , mercari
  , secondStreet
  ) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (LdProduct (..), ldProduct)
import Meguri.Extract.Price
import Meguri.Site.Common
import Meguri.Site.Store (compact, visibleText)
import Meguri.Text (digitsOnly, squish)
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

-- | 在庫の表示（店ごとの言い回し）を読む
stockFromWords :: Text -> Maybe Stock
stockFromWords t
  | has ["在庫なし", "在庫切れ", "販売終了", "販売休止", "完売", "お取り扱いできません", "売り切れ"] = Just OutOfStock
  | has ["お取り寄せ", "取り寄せ", "予約", "入荷時期", "以内に発送", "入荷次第"] = Just Backorder
  | has ["在庫あり", "在庫残少", "在庫僅少", "残り", "お届け"] = Just InStock
  | otherwise = Nothing
 where
  has = any (`T.isInfixOf` t)

stockField :: Text -> Maybe Text -> Field Method Stock
stockField why = \case
  Just t | Just s <- stockFromWords t -> gotRaw Body t s
  Just t -> (missing Body "在庫の表示を解釈できなかった") {raw = Just t}
  Nothing -> missing Body why

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
