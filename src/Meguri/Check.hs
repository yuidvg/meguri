-- | check：サイトごとの健康診断（検索1回・個別1件）
--
-- HTTP で読める段のサイトに、見本の検索語で検索を1回、そこから拾った個別ページを1件当てて、
-- カタログの段のとおりに読めるかを見る。止まっているサイトは、入口を1回だけ見る。
-- ブラウザの段のサイトには、既定では当てない（--all を付けたときだけ、検索URLを1回見る）。
module Meguri.Check
  ( CheckOpts (..)
  , checkAll
  , report
  , itemLinks
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Meguri.Catalog
import Meguri.Extract.Html (Tags, absUrl, attr, parse)
import Meguri.Fetch.Encoding (decodeBody)
import Meguri.Fetch.Http
import Meguri.Health (Health (..))
import Meguri.Item
import Meguri.Sanitize (usableUrl)
import Meguri.List (searchUrl)
import Meguri.Render (blockShort)
import Meguri.Site.Common
import Meguri.Sites
import Meguri.Text (clean, decodeEntities, jstStamp)
import Meguri.Types
import Protolude

data CheckOpts = CheckOpts
  { -- | ブラウザの段のサイトにも、検索URLを1回だけ当てる
    everything :: Bool
  , only :: [Text]
  }

checkAll :: Fetcher -> CheckOpts -> IO [Health]
checkAll f o = do
  day <- T.take 10 . jstStamp <$> getCurrentTime
  mapConcurrently (checkSite f day o.everything) (filter (\s -> null o.only || s.sid `elem` o.only) catalog)

-- | 検索結果のHTMLから、個別ページの形に合うリンクを拾う（アダプタの無いサイト用。純粋）
itemLinks :: Channel -> Text -> Tags -> [Text]
itemLinks s base ts =
  ordNub
    [ u
    | t <- ts
    , Just h <- [attr "href" t]
    , let u = absUrl base (decodeEntities h)
    , usableUrl u
    , matchesItem s u
    ]

checkSite :: Fetcher -> Text -> Bool -> Channel -> IO Health
checkSite f day everything s = case s.tier of
  TierOutOfScope -> pure (h "対象外" "—" True)
  TierStopped _ -> case s.home of
    Nothing -> pure (h "停止中（入口のURLなし）" "—" True)
    Just u -> do
      r <- fetchPage f u
      pure $ case r of
        Left (FetchBlocked b) -> h ("入口：ブロック（" <> blockShort b <> "）") "—" True
        Left (FetchFailed e) -> h ("入口：つながらない（" <> e <> "）") "—" True
        Right p -> case classify p.status (text p) of
          PageStopped why -> h ("入口：停止中のまま（" <> why <> "）") "—" True
          PageOk -> h "入口：HTTP 200（再開したかもしれない。開いて確かめる）" "—" False
          PageNotFound -> h "入口：HTTP 404" "—" True
          PageError why -> h ("入口：" <> why) "—" True
  t
    | browserTier t && not everything -> pure (h "ブラウザの段（HTTPでは当てない）" "—" True)
    | otherwise -> probe
 where
  h search item ok = Health {sid = s.sid, day, search = clean 90 search, item = clean 90 item, asExpected = ok}
  text p = snd (decodeBody p.contentType p.body)
  browserTier t = t `elem` [BrowserJs, BrowserNormal, TierBlocked]
  http = s.tier == Http
  adapter = adapterFor s
  kind = if isJust s.searchNew then New else Used
  target = searchUrl s.enc <$> (s.searchNew <|> s.searchUsed) <*> Just s.sample

  probe = do
    (searchText, links, searchOk) <- case target of
      Nothing -> pure ("検索：URLなし", [], True)
      Just u -> do
        r <- fetchPage f u
        pure $ case r of
          Left (FetchBlocked b) -> ("検索：ブロック（" <> blockShort b <> "）", [], browserTier s.tier)
          Left (FetchFailed e) -> ("検索：つながらない（" <> e <> "）", [], not http && s.tier /= HttpItem)
          Right p ->
            let body = text p
                ts = parse body
             in case (classify p.status body, adapter.listX) of
                  (PageStopped why, _) -> ("検索：停止中（" <> why <> "）", [], False)
                  (PageError why, _) -> ("検索：" <> why, [], browserTier s.tier)
                  (cls, Just ex) | http ->
                    let out = ex (ListCtx s.sid s.sample kind u p.status (jstStamp p.fetchedAt)) ts body
                        urls = [o.url | o <- out.offers]
                     in if
                          | not (null urls) -> ("検索：取得" <> show (length urls) <> "件", urls, True)
                          | out.zero -> ("検索：0件（見本の語「" <> s.sample <> "」で出ない）", [], False)
                          | cls == PageNotFound -> ("検索：HTTP 404", [], False)
                          | otherwise -> ("検索：1件も抜き出せない（作りが変わったかもしれない）", [], False)
                  (PageNotFound, _) -> ("検索：HTTP 404", [], browserTier s.tier)
                  (PageOk, _) ->
                    let ls = itemLinks s u ts
                     in if
                          | browserTier s.tier && not (null ls) -> ("検索：HTTP 200・商品リンク" <> show (length ls) <> "（HTTPで読めるようになったかもしれない）", ls, False)
                          | browserTier s.tier -> ("検索：HTTP 200（商品リンクなし）", [], True)
                          | null ls -> ("検索：HTTP 200（商品リンクなし）", [], not http)
                          | otherwise -> ("検索：HTTP 200・商品リンク" <> show (length ls), ls, True)
    (itemText, itemOk) <- case head links <|> s.sampleItem of
      Nothing -> pure ("個別：見本なし", True)
      Just u -> do
        r <- readItem f u
        pure $ case r of
          Right rec -> case rec.confirmation of
            Confirmed -> ("個別：確認済み", True)
            Partial ms ->
              let missing = filter (not . T.isPrefixOf "アダプタ") ms
               in ( "個別：一部" <> (if null missing then "" else "（足りない：" <> T.intercalate "・" missing <> "）") <> (if rec.adapter then "" else "・汎用の抽出")
                  , True
                  )
            Unconfirmed -> ("個別：何も読めない", False)
          Left (ItemUsage _) -> ("個別：HTTPでは読まない", True)
          Left (ItemBlocked b) -> ("個別：ブロック（" <> blockShort b <> "）", browserTier s.tier)
          Left (ItemFetch e) -> ("個別：" <> e, browserTier s.tier)
    pure (h searchText itemText (searchOk && itemOk))

-- | 診断の表：1サイト1行
report :: [Health] -> Text
report hs =
  T.unlines $
    ("健康診断 " <> show (length hs) <> "サイト：段のとおり " <> show (length (filter (.asExpected) hs)) <> "・要確認 " <> show (length (filter (not . (.asExpected)) hs)))
      : "判定\tid\t段\t検索\t個別"
      : [ T.intercalate "\t" [if x.asExpected then "○" else "要確認", x.sid, maybe "" (tierLabel . (.tier)) (lookupSite x.sid), x.search, x.item]
        | x <- hs
        ]
