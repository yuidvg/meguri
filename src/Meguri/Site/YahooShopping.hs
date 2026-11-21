-- | Yahoo!ショッピング。2nd STREET の Yahoo!店（店内検索）も同じアダプタで読む。
module Meguri.Site.YahooShopping (adapter) where

import Data.Aeson (Value (..))
import Data.List (lookup, nubBy)
import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (at, atPath, int, str)
import Meguri.Extract.NextData (pageProps)
import Meguri.Site.Common
import Meguri.Text (digitsOnly)
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

-- | 一覧。値は data-beacon（_cl_link:title の方）に入っている。
listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html =
  ListOut
    { offers = nubBy ((==) `on` (.url)) (mapMaybe one beacons)
    , zero = textHas ["に一致する商品は見つかりませんでした", "条件に一致する商品は見つかりませんでした", "一致する商品はありません"] html
    , follow = []
    }
 where
  beacons =
    [ kvPairs b
    | t <- ts
    , Just b <- [attr "data-beacon" t <|> attr "data-cl-params" t]
    , "_cl_link:title" `T.isInfixOf` b
    ]
  -- 「中古」の印（_cl_link:usedlbl）は、商品と同じ _cl_position を持つ
  usedPositions =
    [ pos
    | t <- ts
    , Just b <- [attr "data-beacon" t <|> attr "data-cl-params" t]
    , "_cl_link:usedlbl" `T.isInfixOf` b
    , Just pos <- [lookup "_cl_position" (kvPairs b)]
    ]
  one kv = do
    targ <- lookup "targurl" kv
    let store = lookup "storeid" kv
        code = lookup "itemcode" kv
        href = "https://" <> targ
        base = listOffer ctx href
    pure
      base
        { name = slMaybe "商品名が読めなかった" (lookup "tname" kv)
        , priceIncl = slMaybe "価格が読めなかった" (Yen <$> (readMaybe =<< lookup "prc" kv))
        , shipping = missing SearchList "一覧からは読めない（個別ページで確かめる）"
        , seller = withNote "店ID（店名は個別ページで読む）" (slMaybe "店IDが読めなかった" store)
        , image = slMaybe "画像が読めなかった" ((\s c -> "https://item-shopping.c.yimg.jp/i/n/" <> s <> "_" <> c) <$> store <*> code)
        , points = withNote "ログインなしの表示（参考）" (slMaybe "ポイントの表示なし" (readMaybe =<< lookup "point" kv))
        , condition = if maybe False (`elem` usedPositions) (lookup "_cl_position" kv) then sl "中古" else missing SearchList "中古の印なし"
        , keys =
            noKeys
              { jan = validJan =<< lookup "jan" kv
              , siteItemId = (\s c -> s <> "_" <> c) <$> store <*> code
              , sharedStockId = secondStreet href
              }
        }
  validJan j = let dg = digitsOnly j in if T.length dg == 13 then Just dg else Nothing

secondStreet :: Text -> Maybe Text
secondStreet u = if "/ssol-shopping/" `T.isInfixOf` u then thirteenDigits u else Nothing

-- | 個別ページ。__NEXT_DATA__ の pageProps から読む。
itemPage :: ItemCtx -> Draft -> Draft
itemPage ctx d = case pageProps ctx.tags of
  Nothing -> d
  Just pp ->
    let p ks = atPath ks pp
        f :: Text -> Maybe a -> Field Method a
        f = bodyField NextData
        taxable = p ["item", "isTaxable"] /= Just (Bool False)
        fee = int =<< p ["postage", "fee"]
        free = p ["postage", "isPostageFree"]
        qty = int =<< p ["item", "stock", "quantity"]
        avail = p ["item", "stock", "isAvailable"]
     in d
          { name = f "item.name が無い" (str =<< p ["item", "name"]) `orField` d.name
          , priceIncl =
              (if taxable then identity else withNote "非課税の商品")
                (f "item.applicablePrice が無い" (Yen <$> (int =<< p ["item", "applicablePrice"])))
          , shipping = case (free, fee) of
              (Just (Bool True), _) -> gotRaw NextData "postage.isPostageFree:true" Free
              (_, Just n) | n > 0 -> withNote ("配送先：" <> fromMaybe "不明" (str =<< p ["postage", "prefecture"])) (got NextData (Fixed (Yen n)))
              _ -> withNote "配送先で変わる" (got NextData ByDestination)
          , stock = case (avail, qty) of
              (Just (Bool False), _) -> got NextData OutOfStock
              (_, Just n) -> gotRaw NextData ("quantity:" <> show n) (if n > 0 then InStock else OutOfStock)
              (Just (Bool True), _) -> got NextData InStock
              _ -> d.stock
          , seller = f "seller.name が無い" (str =<< p ["seller", "name"])
          , buy = case (str =<< p ["cartButton", "displayState"], str =<< p ["cartButton", "text"]) of
              (Just "Active", t) -> got NextData (BuyActive (fromMaybe "カートに入れる" t))
              (Just _, t) -> got NextData (BuyDisabled (fromMaybe "カートに入れる" t))
              _ -> missing NextData "cartButton が無い"
          , points = withNote "ログインなしの表示（参考）" (f "point.totalPoint が無い" (int =<< p ["point", "totalPoint"]))
          , condition = f "状態の表示なし" ((str =<< p ["item", "usedConditionText"]) <|> (str =<< p ["item", "usedCondition"])) `orField` d.condition
          , keys =
              d.keys
                { jan = (validJan =<< str =<< p ["item", "janCode"]) <|> d.keys.jan
                , siteItemId = (str =<< p ["item", "sellerManagedItemId"]) <|> d.keys.siteItemId
                , sharedStockId = secondStreet ctx.pageUrl
                }
          }
 where
  validJan j = let dg = digitsOnly j in if T.length dg == 13 then Just dg else Nothing
  _unused = at
