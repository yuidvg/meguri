-- | 価格.com（検索とショップ一覧）。ここの値は、店のページを見たことにならない。
module Meguri.Site.Kakaku (adapter) where

import Data.List (nubBy)
import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.Price (parseYen)
import Meguri.Sanitize (usableUrl)
import Meguri.Site.Common
import Meguri.Text (queryParam)
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) Nothing

listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts html
  | "/shop/" `T.isInfixOf` ctx.pageUrl = found (shops ctx ts)
  -- シリーズ（色違いをまとめたページ）。色ごとの製品のショップ一覧へ進む
  | "/item/J" `T.isInfixOf` ctx.pageUrl = ListOut [] False (map shopUrl (take 3 (variantIds html)))
  | otherwise =
      let os = mapMaybe (searchItem ctx) (subtrees (hasClass "p-resultItem") ts)
          -- JAN などで探すと、製品ページではなく、モールの出品の一覧（ショッピング検索。リンクは /ksearch/jump/）が出る。
          -- 読むのは製品ページだけなので、0件とする（モールの出品は、楽天や Yahoo! の検索で直接読める）
          mallOnly = "https://kakaku.com/ksearch/jump/" `T.isInfixOf` html
       in ListOut
            { offers = os
            , zero = null os && (mallOnly || textHas ["に一致する商品は見つかりませんでした", "該当する製品はありません", "見つかりませんでした"] html)
            , -- 先頭の製品（シリーズならその色違い）のショップ一覧を続けて取る
              follow = [if "/item/K" `T.isInfixOf` o.url then o.url <> "shop/" else o.url | o <- take 1 os]
            }

shopUrl :: Text -> Text
shopUrl k = "https://kakaku.com/item/" <> k <> "/shop/"

-- | シリーズのページにある、色ごとの製品の番号（@var prdlprcK0001441330 = …@）
variantIds :: Text -> [Text]
variantIds html = ordNub [T.takeWhile isAlphaNum (T.drop (T.length "var prdlprc") hit) | hit <- drop 1 (T.splitOn "var prdlprc" ("x" <> html)) >>= \s -> ["var prdlprc" <> s]]

-- | 検索結果のうち、価格.com の製品ページ（/item/K… か /item/J…）に行くもの
searchItem :: ListCtx -> Tags -> Maybe (Offer ListSource)
searchItem ctx el = do
  nameEl <- head (subtrees (hasClass "p-item_name") el)
  href <- attr "href" =<< find (isJust . attr "href") nameEl
  guard ("https://kakaku.com/item/" `T.isPrefixOf` href)
  let maker = textOfClass "p-item_maker" el
      title = textOfClass "p-item_name" el
      priceText = textOfClass "p-item_priceNum" el
      pid = T.takeWhile (/= '/') (T.drop (T.length "https://kakaku.com/item/") href)
  pure
    (listOffer ctx href)
      { name = slMaybe "製品名が読めなかった" (T.unwords . catMaybes <$> Just [maker, title])
      , priceIncl = case priceText of
          Just t | Just y <- parseYen t -> withNote "価格.com の最安値" (gotRaw SearchList t y)
          _ -> missing SearchList "価格の表示なし"
      , shipping = missing SearchList "店ごとに違う（ショップ一覧で見る）"
      , seller = missing SearchList "価格.com の製品ページ（店は未確認）"
      , image = slMaybe "画像なし" (fullscale pid)
      , keys = noKeys {siteItemId = Just pid, model = title}
      }

fullscale :: Text -> Maybe Text
fullscale pid
  | "K" `T.isPrefixOf` pid = Just ("https://img1.kakaku.k-img.com/images/productimage/fullscale/" <> pid <> ".jpg")
  | otherwise = Nothing

-- | ショップ一覧。「売り場へ行く」の onclick にある k3c.atrack.ping({...}) を読む。
-- 同じ店がタブ違いで重複して出るので、店と価格で1つにまとめる。
shops :: ListCtx -> Tags -> [Offer ListSource]
shops ctx ts =
  nubBy ((==) `on` (\o -> (o.seller.value, o.priceIncl.value))) $
    [ (listOffer ctx (shopPage (fromMaybe ctx.pageUrl (attr "href" t))))
      { name = slMaybe "製品名が読めなかった" (jsString "prdname" oc)
      , priceIncl = slMaybe "価格が読めなかった" (Yen <$> jsNumber "shpbid" oc)
      , shipping = case jsNumber "shpshp" oc of
          Just 0 -> gotRaw SearchList "shpshp:0" Free
          Just n -> gotRaw SearchList ("shpshp:" <> show n) (Fixed (Yen n))
          Nothing -> missing SearchList "送料の表示なし（店のページで確かめる）"
      , seller = slMaybe "店名が読めなかった" (jsString "shpname" oc)
      , image = slMaybe "画像なし" (fullscale =<< jsString "prdkey" oc)
      , points = withNote "価格.com の表示（参考）" (slMaybe "ポイントの表示なし" (jsNumber "shppnt" oc))
      , keys = noKeys {siteItemId = jsString "prdkey" oc, model = jsString "prdname" oc}
      }
    | t <- ts
    , Just oc <- [attr "onclick" t]
    , "shpname:'" `T.isInfixOf` oc
    , isJust (jsNumber "shpbid" oc)
    ]

-- | 「売り場へ行く」の転送URL（c.kakaku.com/forwarder）の Url に、店のページのURLが入っている。
-- それが店のページを直接指していれば、そちらを使う。
-- アフィリエイトの中継（URLの中に別のURLが入っている）なら、転送URLのままにする。
shopPage :: Text -> Text
shopPage href = case queryParam "Url" href of
  Just u
    | usableUrl u && not (nested u) && not (relay u) -> u
  _ -> href
 where
  -- 中継だけをするホスト（アフィリエイト）
  relay u = any (\h -> ("://" <> h <> "/") `T.isInfixOf` u) ["click.linksynergy.com", "linksynergy.jrs5.com", "hb.afl.rakuten.co.jp", "ck.jp.ap.valuecommerce.com", "prf.hn", "px.a8.net", "af.moshimo.com", "t.afi-b.com", "kakaku.com"]
  nested u = any (`T.isInfixOf` T.toLower (T.drop 8 u)) ["http:", "https:", "http%3a", "https%3a", "http%253a", "https%253a"]

-- | JS のオブジェクトの @key:'value'@
jsString :: Text -> Text -> Maybe Text
jsString key src =
  let (_, hit) = T.breakOn (key <> ":'") src
   in if T.null hit then Nothing else Just (T.takeWhile (/= '\'') (T.drop (T.length key + 2) hit))

-- | JS のオブジェクトの @key:123@
jsNumber :: Text -> Text -> Maybe Int
jsNumber key src =
  let (_, hit) = T.breakOn (key <> ":") src
      ds = T.takeWhile isDigit (T.drop (T.length key + 1) hit)
   in if T.null hit || T.null ds then Nothing else readMaybe ds
