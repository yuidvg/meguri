-- | ラクマ
module Meguri.Site.Rakuma (adapter) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Site.Common
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html =
  ListOut
    { offers = mapMaybe one (subtrees (hasClass "item-box") ts)
    , zero = textHas ["に一致する商品は見つかりませんでした", "該当する商品が見つかりません", "見つかりませんでした"] html
    , follow = []
    }
 where
  one el = do
    link <- find (hasClass "link_search_title") el
    href <- attr "href" link
    let price = Yen <$> (readMaybe =<< find (T.all (`elem` ("0123456789" :: [Char]))) (mapMaybe (attr "data-content") el))
        img = find (isJust . attr "data-original") el >>= attr "data-original"
        sold = any (\t -> any ("sold" `T.isInfixOf`) (map T.toLower (classes t))) el
    pure
      (listOffer ctx href)
        { name = slMaybe "商品名が読めなかった" (firstTextWhere (hasClass "link_search_title") el)
        , priceIncl = slMaybe "価格が読めなかった" price
        , shipping = missing SearchList "一覧に送料の表示なし（個別ページで確かめる）"
        , stock = sl (if sold then Sold else Selling)
        , seller = missing SearchList "一覧に出品者名なし"
        , image = slMaybe "画像なし" img
        , keys = noKeys {siteItemId = attr "data-rat-itemid" link}
        }

-- | 売り切れても JSON-LD は InStock のまま。本文の SOLD OUT と「購入に進む」で判断する。
itemPage :: ItemCtx -> Offer Method -> Offer Method
itemPage ctx d =
  d
    { priceIncl = d.priceIncl {note = Just "フリマの表示価格（これが支払う額）"}
    , stock = case (soldOut, hasBuy) of
        (True, _) -> gotRaw Body "SOLD OUT" Sold
        (False, True) -> gotRaw Body "購入に進む" Selling
        _ -> missing Body "SOLD OUT も「購入に進む」も無い"
    , buy = if hasBuy && not soldOut then got Body (BuyActive "購入に進む") else if soldOut then got Body (BuyDisabled "SOLD OUT") else missing Body "購入ボタンが無い"
    , shipping = case () of
        _
          | any (hasClass "postage-included") ctx.tags -> gotRaw Body "送料込" Free
          | textHas ["着払い"] ctx.html -> gotRaw Body "着払い" BuyerPays
          | otherwise -> missing Body "送料の表示なし"
    , keys = d.keys {siteItemId = Just (T.takeWhileEnd (/= '/') (stripQuery ctx.pageUrl))}
    }
 where
  soldOut = textHas ["SOLD OUT", "sold-out"] ctx.html
  hasBuy = any (hasClass "btn_buy") ctx.tags
