-- | ハードオフ（オフモール）
module Meguri.Site.HardOff (adapter) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (LdProduct (..), ldProduct)
import Meguri.Extract.Price
import Meguri.Site.Common
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

-- | 0件のときは HTTP 404 が返る
listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts _ =
  ListOut
    { offers = mapMaybe one (subtrees (hasClass "itemcolmn_item") ts)
    , zero = ctx.httpStatus == 404
    , follow = []
    }
 where
  one el = do
    href <- attr "href" =<< find (maybe False ("/product/" `T.isInfixOf`) . attr "href") el
    let brand = textOfClass "item-brand-name" el
        title = textOfClass "item-name" el
        code = textOfClass "item-code" el
        img = find (\t -> isJust (attr "alt" t) && maybe False ("http" `T.isPrefixOf`) (attr "src" t)) el
        rank = attr "alt" =<< find (maybe False ("rank" `T.isInfixOf`) . attr "src") el
    pure
      (listOffer ctx href)
        { name = slMaybe "商品名が読めなかった" (Just (T.unwords (catMaybes [brand, title, code])))
        , priceIncl = withNote "一覧の表示（税込）" (slMaybe "価格が読めなかった" (parseYen =<< textOfClass "item-price-en" el))
        , shipping = withNote "配送先で変わる" (sl ByDestination)
        , stock = sl Selling
        , seller = missing SearchList "一覧に取扱店舗なし"
        , image = slMaybe "画像なし" (attr "src" =<< img)
        , condition = slMaybe "ランクの表示なし" (("ランク " <>) . T.toUpper <$> rank)
        , keys = noKeys {model = code, siteItemId = Just (T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') href))}
        }

-- | JSON-LD の価格は税抜。税込価格とランクは本文にある。
itemPage :: ItemCtx -> Offer Method -> Offer Method
itemPage ctx d =
  d
    { name = fmap (\n -> T.unwords (catMaybes [brand, Just n, modelCode])) d.name
    , priceExcl = withNote "JSON-LD の価格は税抜" (d.priceIncl {note = Nothing})
    , priceIncl = case (textOfClass "product-detail-price__main" ctx.tags, textOfClass "product-detail-price__sub" ctx.tags) of
        (Just p, Just sub)
          | taxMark sub == Just MarkIncl, Just y <- parseYen p -> gotRaw Body (p <> " " <> sub) y
        _ -> missing Body "本文の税込価格が読めなかった"
    , shipping = withNote "都道府県ごと。MEGURI_PREF を設定すると、配送先までの送料を入れる" (got Body ByDestination)
    , seller = bodyField Body "取扱店舗が読めなかった" (textOfClass "product-detail-store__name" ctx.tags <|> storeFromLink)
    , buy = case find (\t -> hasClass "product-detail-cart-btn" t || attr "id" t == Just "cartIn") ctx.tags of
        Just _ -> got Body (BuyActive "カートに入れる")
        Nothing
          | textHas ["カートに入れる"] ctx.html -> got Body (BuyActive "カートに入れる")
          | textHas ["SOLD OUT", "売り切れ"] ctx.html -> got Body (BuyDisabled "売り切れ")
          | otherwise -> missing Body "カートのボタンが無い"
    , condition = bodyField Body "ランクの表示なし" (find (" RANK" `T.isSuffixOf`) (mapMaybe (attr "alt") ctx.tags))
    , keys = d.keys {model = modelCode <|> d.keys.model}
    }
 where
  brand = ldProduct ctx.tags >>= (.brand)
  -- 「型番：WH-1000XM4」
  modelCode = mfilter (not . T.null) (T.strip . T.takeWhileEnd (/= '：') <$> textOfClass "product-detail-num" ctx.tags)
  storeFromLink = firstTextWhere (\t -> maybe False ("/shop/" `T.isInfixOf`) (attr "href" t) && hasClassPrefix "product-detail-store" t) ctx.tags
