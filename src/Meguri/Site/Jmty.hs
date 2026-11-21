-- | ジモティー
module Meguri.Site.Jmty (adapter) where

import Data.Aeson (Value (..))
import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (arr, at, atPath, int, str)
import Meguri.Extract.NextData (pageProps)
import Meguri.Extract.Price (parseYen)
import Meguri.Site.Common
import Meguri.Types
import Protolude
import Text.HTML.TagSoup (Tag (..))

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

-- | 「受付終了投稿一覧」の見出しより後ろは受付終了なので除く
listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html =
  let open = takeWhile (not . isClosedHeading) ts
      os = mapMaybe one (subtrees (hasClass "p-articles-list-item") open)
   in ListOut
        { offers = os
        , zero = null os && textHas ["見つかりませんでした", "該当する投稿はありません", "0件"] html
        , follow = []
        }
 where
  isClosedHeading = \case
    TagText t -> "受付終了投稿一覧" `T.isInfixOf` t
    _ -> False
  one el = do
    titleEl <- head (subtrees (hasClass "p-item-title") el)
    href <- attr "href" =<< find (isJust . attr "href") titleEl
    let img = find (hasClass "p-item-image") el
        priceText = textOfClass "p-item-most-important" el
        area = textOfClass "p-item-secondary-important" el
    pure
      (listOffer ctx href)
        { name = slMaybe "題名が読めなかった" (textOfClass "p-item-title" el)
        , priceIncl = case priceText of
            Just t | Just y <- parseYen t -> gotRaw SearchList t y
            Just t -> (missing SearchList "価格の表示が金額でない") {raw = Just t}
            Nothing -> missing SearchList "価格の表示なし"
        , shipping = withNote ("手渡しが基本" <> maybe "" ("（" <>) ((<> "）") <$> area)) (sl BuyerPays)
        , stock = sl Selling
        , seller = missing SearchList "一覧に投稿者名なし"
        , image = slMaybe "画像なし" (attr "src" =<< img)
        , keys = noKeys {siteItemId = Just (T.takeWhileEnd (/= '-') href)}
        }

itemPage :: ItemCtx -> Offer Method -> Offer Method
itemPage ctx d = case atPath ["articleResults", "article"] =<< pageProps ctx.tags of
  Nothing -> d
  Just a ->
    let closed = at "closed" a == Just (Bool True) || at "destroyed" a == Just (Bool True)
        bySeller = atPath ["delivery_option_for_online_purchase", "by_seller"] a == Just (Bool True)
        area = str =<< (atPath ["map", "area_name"] =<< head (maybe [] arr (at "locations" a)))
        price = (int =<< atPath ["par_category_items", "price"] a) <|> (unYen <$> (parseYen =<< str =<< at "important_field" a))
        imgUrl = str =<< atPath ["ogImage"] =<< pageProps ctx.tags
     in d
          { name = bodyField NextData "title が無い" (str =<< at "title" a) `orField` d.name
          , priceIncl = bodyField NextData "価格が読めなかった" (Yen <$> price)
          , shipping =
              withNote ("受け渡し場所：" <> fromMaybe "不明" area) $
                got NextData (if bySeller then ShipUnknown "投稿者が配送（額は相談）" else BuyerPays)
          , stock = got NextData (if closed then Sold else Selling)
          , seller = bodyField NextData "投稿者名が読めなかった" ((str =<< atPath ["articleResults", "post_user", "name"] =<< pageProps ctx.tags))
          , image = bodyField NextData "画像なし" imgUrl `orField` d.image
          , buy =
              if closed
                then got NextData (BuyDisabled "受付終了")
                else
                  if textHas ["投稿者にメールで問い合わせ", "問い合わせ"] ctx.html
                    then withNote "問い合わせにはログインが要る" (got Body (BuyActive "投稿者にメールで問い合わせ"))
                    else missing Body "問い合わせのボタンが無い"
          , keys = d.keys {siteItemId = str =<< at "key" a}
          }
