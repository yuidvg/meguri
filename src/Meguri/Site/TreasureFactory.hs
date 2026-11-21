-- | トレジャーファクトリー
module Meguri.Site.TreasureFactory (adapter) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.Price
import Meguri.Site.Common
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

-- | 0件でも200が返る
listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html =
  let os = mapMaybe one (subtrees (hasClass "pj-search_item") ts)
   in ListOut
        { offers = os
        , zero = null os && textHas ["見つかりませんでした", "該当する商品がありません", "0件"] html
        , follow = []
        }
 where
  one el = do
    href <- attr "href" =<< find (hasClass "cm-itemlist_itemcode_link") el
    let img = find (isJust . attr "alt") el
        texts = mapMaybe (\s -> firstTextWhere (const True) s) (subtrees (hasClass "cm-itemlist_text") el)
        code = T.takeWhileEnd (/= '/') href
    pure
      (listOffer ctx (absUrl ctx.pageUrl href))
        { name = slMaybe "商品名が読めなかった" ((attr "alt" =<< img) <|> (if null texts then Nothing else Just (T.unwords texts)))
        , priceIncl = withNote "一覧の表示（税込）" (slMaybe "価格が読めなかった" (parseYen =<< textOfClass "cm-itemlist_price" el))
        , shipping = withNote "配送先で変わる" (sl ByDestination)
        , stock = sl Selling
        , seller = missing SearchList "一覧に出品店舗なし"
        , image = slMaybe "画像なし" (large . absUrl ctx.pageUrl <$> (attr "src" =<< img))
        , keys = noKeys {siteItemId = Just code}
        }

-- | 画像URLの大きさの部分（/340/ や /450/）を /1500/ にする
large :: Text -> Text
large = T.replace "/340/" "/1500/" . T.replace "/450/" "/1500/"

-- | JSON-LD の価格は税抜（本文の小さい方の表示と同じ）。税込は本文の大きい方。
itemPage :: ItemCtx -> Offer Method -> Offer Method
itemPage ctx d =
  d
    { priceExcl = withNote "JSON-LD の価格は税抜" (d.priceIncl {note = Nothing})
    , priceIncl = case textOfClass "pj-item_detail_pricetext" ctx.tags of
        Just t | Just y <- parseYen t -> withNote "本文の大きい表示（税込）" (gotRaw Body t y)
        _ -> missing Body "本文の税込価格が読めなかった"
    , shipping = withNote "都道府県ごと。MEGURI_PREF を設定すると、配送先までの送料を入れる" (got Body ByDestination)
    , image = fmap large d.image
    , buy = case find (\t -> attr "id" t == Just "item-cartbutton") ctx.tags of
        Just _ -> got Body (BuyActive "カートに入れる")
        Nothing -> if textHas ["SOLD OUT", "売り切れ"] ctx.html then got Body (BuyDisabled "売り切れ") else missing Body "カートのボタンが無い"
    , keys = d.keys {siteItemId = Just (T.takeWhileEnd (/= '/') (stripQuery ctx.pageUrl))}
    }
