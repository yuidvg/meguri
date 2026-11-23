-- | item：個別ページを読み、基準の項目を出どころつきで出す
module Meguri.Item
  ( extractItem
  , readItem
  , readItemShipped
  , ItemError (..)
  ) where

import Meguri.Catalog (Channel (..), Tier (..), tierLabel)
import Meguri.Confirm (toReading)
import Meguri.Extract.Generic (genericDraft)
import Meguri.Extract.Html (parse)
import Meguri.Fetch.Encoding (decodeBody)
import Meguri.Fetch.Http
import Meguri.Sanitize (tidyOffer)
import Meguri.Ship (Pref, ShipPlan (..), applyShipping, shipPlan)
import Meguri.Site.Common
import Meguri.Sites
import Meguri.Text (jstStamp)
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

data ItemError
  = -- | 取りに行く前に分かること（カタログで、HTTP では読めないと分かっている販路）
    ItemUsage Text
  | -- | ブロックされた
    ItemBlocked Block
  | -- | 取りに行って失敗した
    ItemFetch Text

-- | 個別ページを取りに行って読む
readItem :: Fetcher -> Text -> IO (Either ItemError Reading)
readItem f url = case siteForUrl url of
  Just c
    | c.tier `elem` [BrowserJs, BrowserNormal] ->
        pure (Left (ItemUsage ("HTTP では読めない販路（カタログ：" <> tierLabel c.tier <> "、" <> show c.checked <> " に確かめた）なので、取りに行かない")))
    | c.tier == TierBlocked ->
        pure (Left (ItemUsage ("どの方法でも読めなかった販路（カタログ、" <> show c.checked <> " に確かめた）なので、取りに行かない")))
  _ -> do
    r <- fetchPage f url
    pure $ case r of
      Left (FetchBlocked b) -> Left (ItemBlocked b)
      Left (FetchFailed e) -> Left (ItemFetch e)
      Right p ->
        let (_, html) = decodeBody p.contentType p.body
         in case classify p.status html of
              PageOk -> Right (extractItem url (jstStamp p.fetchedAt) html)
              PageNotFound -> Left (ItemFetch "HTTP 404（ページが無い。売り切れて消えたかもしれない）")
              PageStopped why -> Left (ItemFetch ("停止中：" <> why))
              PageError why -> Left (ItemFetch why)

-- | 'readItem' に加えて、配送先（MEGURI_PREF）が決まっていて、送料を計算できるサイトなら、
-- 送料を問い合わせて記録に入れる。問い合わせに失敗したら、記録はそのまま返す。
readItemShipped :: Fetcher -> Maybe Pref -> Text -> IO (Either ItemError Reading)
readItemShipped f pref url = do
  r <- readItem f url
  case (r, pref >>= (`shipPlan` url)) of
    (Right rec, Just plan) -> do
      page <- fetchReq f plan.req
      pure . Right $ case page of
        Right p | Just fee <- plan.readFee (snd (decodeBody p.contentType p.body)) -> applyShipping fee rec
        _ -> rec
    _ -> pure r
