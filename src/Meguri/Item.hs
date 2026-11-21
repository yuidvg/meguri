-- | item：個別ページを読み、基準の項目を出どころつきで出す
module Meguri.Item
  ( extractItem
  , readItem
  , readItemShipped
  , ItemError (..)
  ) where

import Data.ByteString qualified as BS
import Data.Time (getCurrentTime)
import Meguri.Catalog (Channel (..), Tier (..))
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

-- | HTMLの文字列 → 記録（純粋）。HTTP で取ったページにも、ブラウザで保存したページにも、
-- fixture にも、同じ関数を使う。
extractItem :: PageOrigin -> Text -> Text -> Text -> Reading
extractItem origin url stamp html =
  let ts = parse html
      def = siteForUrl url
      -- カタログに無いサイト（価格.com から行く店など）は、ホスト名で呼ぶ
      siteId = maybe (hostOf url) (.sid) def
      base = genericDraft siteId url stamp ts
   in case def >>= (.itemX) . adapterFor of
        Just f -> toReading origin True (tidyOffer (f (ItemCtx url ts html) base))
        Nothing -> toReading origin False (tidyOffer base)

data ItemError
  = -- | 取りに行く前に分かる誤り（ブラウザで保存したHTMLが要る、など）
    ItemUsage Text
  | -- | 取りに行って失敗した
    ItemFetch Text

-- | URL を取りに行く。@--html@ があれば、そのファイルを読む（通常のブラウザで保存したHTML）。
readItem :: Fetcher -> Text -> Maybe FilePath -> IO (Either ItemError Reading)
readItem f url saved = case saved of
  Just path -> do
    body <- BS.readFile path
    now <- getCurrentTime
    let (_, html) = decodeBody Nothing body
    pure (Right (extractItem FromCapture url (jstStamp now) html))
  Nothing -> case (.tier) <$> siteForUrl url of
    Just t | t `elem` [BrowserJs, BrowserNormal] -> pure (Left (ItemUsage "このサイトの個別ページはHTTPで読めない。ブラウザで開いてHTMLを保存し、--page URL=FILE（item なら --html FILE）で渡す"))
    Just TierBlocked -> pure (Left (ItemUsage "このサイトはどの方法でも読めなかった（meguri sites --format md の注記を見る）"))
    _ -> do
      r <- fetchPage f url
      pure $ case r of
        Left e -> Left (ItemFetch e)
        Right p ->
          let (_, html) = decodeBody p.contentType p.body
           in case classify p.status html of
                PageOk -> Right (extractItem FromHttp url (jstStamp p.fetchedAt) html)
                PageNotFound -> Left (ItemFetch "HTTP 404（ページが無い。売り切れて消えたかもしれない）")
                PageBlocked why -> Left (ItemFetch ("ブロックされた：" <> why <> "。通常のブラウザで開いてHTMLを保存し、--page URL=FILE（item なら --html FILE）で渡す"))
                PageStopped why -> Left (ItemFetch ("停止中：" <> why))
                PageError why -> Left (ItemFetch why)

-- | 'readItem' に加えて、配送先（MEGURI_PREF）が決まっていて、送料を計算できるサイトなら、
-- 送料を問い合わせて記録に入れる。問い合わせに失敗したら、記録はそのまま返す。
readItemShipped :: Fetcher -> Maybe Pref -> Text -> Maybe FilePath -> IO (Either ItemError Reading)
readItemShipped f pref url saved = do
  r <- readItem f url saved
  case (r, pref >>= (`shipPlan` url)) of
    (Right rec, Just plan) -> do
      page <- fetchReq f plan.req
      pure . Right $ case page of
        Right p | Just fee <- plan.readFee (snd (decodeBody p.contentType p.body)) -> applyShipping fee rec
        _ -> rec
    _ -> pure r
