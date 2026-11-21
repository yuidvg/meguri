-- | 楽天市場。支店（ジョーシン・エディオン・2nd STREET）も同じアダプタで読む。
module Meguri.Site.Rakuten (adapter) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.Microdata (itemprop)
import Meguri.Extract.Price
import Meguri.Site.Common
import Meguri.Text (squish)
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html =
  ListOut
    { offers = mapMaybe one (subtrees (hasClass "searchresultitem") ts)
    , zero = textHas ["に一致する商品は見つかりませんでした", "該当する商品はみつかりませんでした", "該当する商品が見つかりませんでした"] html
    , follow = []
    }
 where
  one el = do
    link <- find (\t -> attr "data-link" t == Just "item") el
    href <- stripQuery <$> attr "href" link
    let title = attr "title" link <|> firstTextWhere (\t -> attr "data-link" t == Just "item") el
        priceEl = find (isJust . attr "data-price") el
        yen = Yen <$> (priceEl >>= attr "data-price" >>= readMaybe)
        taxIncl = (priceEl >>= attr "data-tax-included") /= Just "false"
        shipText = firstTextWhere (hasClassPrefix "free-shipping") el <|> find ("送料" `T.isInfixOf`) (shortTexts el)
        shop = firstTextWhere (hasClass "merchant") el
        img = T.takeWhile (/= '?') <$> (find (isJust . attr "src") (elementsWhere (\t -> isJust (attr "alt" t)) el) >>= attr "src")
        pts = priceEl >>= const (pointsIn el)
        base = listOffer ctx href
    pure
      base
        { name = slMaybe "商品名が読めなかった" title
        , priceIncl = if taxIncl then slMaybe "価格が読めなかった" yen else missing SearchList "税抜の表示"
        , priceExcl = if taxIncl then missing SearchList "税込で表示" else slMaybe "価格が読めなかった" yen
        , shipping = case shipText of
            Just s | Just v <- parseShipping s -> gotRaw SearchList s v
            Just s -> (missing SearchList "送料の表示を解釈できなかった") {raw = Just s}
            Nothing -> missing SearchList "一覧に送料の表示なし（個別ページで確かめる）"
        , seller = slMaybe "店名が読めなかった" shop
        , image = slMaybe "画像が読めなかった" img
        , points = withNote "ログインなしの表示（参考）" (slMaybe "ポイントの表示なし" pts)
        , keys = noKeys {siteItemId = find (isJust . attr "data-track-itemid") el >>= attr "data-track-itemid", sharedStockId = secondStreet href}
        }
  shortTexts el = filter (\s -> T.length s < 30) (map squish (T.lines (innerText el)))
  pointsIn el = do
    box <- head (subtrees (\t -> attr "data-component" t == Just "bdg-point-display") el)
    readMaybe . T.filter (/= ',') =<< find (T.all (\c -> c `elem` ("0123456789," :: [Char]))) (filter (not . T.null) (map squish (T.lines (innerText box))))

secondStreet :: Text -> Maybe Text
secondStreet u = if "/jumblestore/" `T.isInfixOf` u then thirteenDigits u else Nothing

-- | 個別ページ（EUC-JP）。価格と在庫は microdata、店名と送料は埋め込みの JSON。
itemPage :: ItemCtx -> Draft -> Draft
itemPage ctx d =
  d
    { name = fmap stripShop d.name
    , priceIncl = if taxIncl then price else missing Microdata "税抜の表示"
    , priceExcl = if taxIncl then missing Microdata "税込で表示" else price
    , shipping = case scanJsonBool "postageIncluded" ctx.html of
        Just True -> gotRaw EmbeddedJson "postageIncluded:true" Free
        Just False -> withNote "送料別。額は配送先で変わり、HTMLに無い。店の送料無料ライン（一覧に「送料無料」と出ることがある）は店のページで確かめる" (gotRaw EmbeddedJson "postageIncluded:false" ByDestination)
        Nothing -> missing EmbeddedJson "postageIncluded が無い"
    , seller = bodyField EmbeddedJson "shopName が無い" (scanJsonString "shopName" ctx.html)
    , buy = case scanJsonBool "displayNormalCartButton" ctx.html of
        Just True -> got EmbeddedJson (BuyActive "かごに追加")
        Just False -> got EmbeddedJson (BuyDisabled "かごに追加")
        Nothing -> missing EmbeddedJson "displayNormalCartButton が無い"
    , condition = bodyField EmbeddedJson "itemCondition が無い" (scanJsonString "itemCondition" ctx.html)
    , keys = d.keys {sharedStockId = secondStreet ctx.pageUrl}
    }
 where
  taxIncl = scanJsonBool "taxIncluded" ctx.html /= Just False
  price = bodyField Microdata "itemprop=price が無い" (parseYen =<< itemprop "price" ctx.tags)
  stripShop t = case T.breakOnEnd "｜" t of
    (a, _) | not (T.null a) -> T.dropEnd 1 a
    _ -> t
