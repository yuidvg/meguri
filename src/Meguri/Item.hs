-- | item：個別ページを読み、基準の項目を出どころつきで出す
module Meguri.Item
  ( extractItem
  , readItem
  , readItemShipped
  , ItemResult (..)
  ) where

import Meguri.Catalog (Channel (..), Tier (..), tierLabel)
import Meguri.Confirm (toReading)
import Meguri.Extract.Generic (genericDraft)
import Data.Time (UTCTime)
import Meguri.Extract.Html (parse)
import Meguri.Fetch.Http
import Meguri.Sanitize (tidyOffer)
import Meguri.Ship (Pref, ShipPlan (..), applyShipping, shipPlan)
import Meguri.Site.Common
import Meguri.Sites
import Meguri.Types
import Protolude

-- | HTMLの文字列 → 記録（純粋）。HTTP で取ったページにも fixture にも、同じ関数を使う。
extractItem :: Text -> Text -> Text -> Reading
extractItem url stamp html =
  let ts = parse html
      def = siteForUrl url
      -- カタログに無いサイト（価格.com から行く店など）は、ホスト名で呼ぶ
      siteId = maybe (hostOf url) (.sid) def
      base = genericDraft siteId url stamp ts
   in case def >>= (.itemX) . adapterFor of
        Just f -> toReading True (tidyOffer (f (ItemCtx url ts html) base))
        Nothing -> toReading False (tidyOffer base)

-- | 個別ページを読みに行った結果
data ItemResult
  = -- | 取りに行かなかった（カタログで、HTTP では読めないと分かっている販路）
    ItemSkipped Skip
  | -- | 取りに行ったが、読めなかった。その取得
    ItemUnread [Attempt]
  | -- | 読めた。取得は、個別ページと、送料を問い合わせたならその問い合わせ
    ItemRead Reading [Attempt]

-- | 個別ページを取りに行って読む
readItem :: Fetcher -> Maybe UTCTime -> Text -> IO ItemResult
readItem f deadline url = case siteForUrl url of
  Just c
    | c.tier `elem` [BrowserJs, BrowserNormal] ->
        pure (ItemSkipped (NeedsBrowser ("HTTP では読めない販路（カタログ：" <> tierLabel c.tier <> "、" <> show c.checked <> " に確かめた）なので、取りに行かない")))
    | c.tier == TierBlocked ->
        pure (ItemSkipped (BlockedBefore (show c.checked) "どの方法でも読めなかった販路（カタログ）なので、取りに行かない"))
  _ -> do
    (a, loaded) <- attempt f deadline DetailPage (page url)
    pure $ case (a.outcome, loaded) of
      (Got, Just p) -> ItemRead (extractItem url p.at p.text) [a]
      _ -> ItemUnread [a]

-- | 'readItem' に加えて、配送先（MEGURI_PREF）が決まっていて、送料を計算できるサイトなら、
-- 送料を問い合わせて記録に入れる。
--
-- 問い合わせを取れなければ、記録の送料はページの値のままで、問い合わせの取得がその結果を持つ。
-- 応答は取れたが送料を読めなければ、送料の項目の注記に、そう書く。
readItemShipped :: Fetcher -> Maybe UTCTime -> Maybe Pref -> Text -> IO ItemResult
readItemShipped f deadline pref url = do
  r <- readItem f deadline url
  case (r, pref >>= (`shipPlan` url)) of
    (ItemRead rec as, Just plan) -> do
      (a, loaded) <- attempt f deadline ShipQuery plan.req
      pure $ case loaded >>= \p -> plan.readFee p.text of
        Just fee -> ItemRead (applyShipping fee rec) (as <> [a])
        Nothing | isJust loaded -> ItemRead (unreadFee rec) (as <> [a])
        Nothing -> ItemRead rec (as <> [a])
    _ -> pure r
 where
  unreadFee rec =
    let o = rec.offer {shipping = withNote "送料の問い合わせの応答から、送料を読めなかった" rec.offer.shipping}
     in Reading {offer = o, adapter = rec.adapter, confirmation = rec.confirmation}
