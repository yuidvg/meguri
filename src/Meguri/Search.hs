-- | search：全サイトを並列に当たり、候補（未確認）と「どこまで探したか」を作る
module Meguri.Search
  ( KindSel (..)
  , SearchOpts (..)
  , Job (..)
  , plan
  , searchUrl
  , runSearch
  , runJobs
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVarIO)
import Data.Set qualified as Set
import Data.Text qualified as T
import Meguri.Catalog
import Data.Time (getCurrentTime)
import Meguri.Extract.Html (Tags, parse)
import Meguri.Fetch.Encoding
import Meguri.Fetch.Http
import Meguri.Sanitize (tidyOffer, usableUrl)
import Meguri.Site.Common
import Meguri.Sites
import Meguri.Text (jstStamp, percentEncode)
import Meguri.Types
import Protolude

data KindSel = SelNew | SelUsed | SelAll
  deriving stock (Eq, Show)

data SearchOpts = SearchOpts
  { queries :: [Text]
  , kindSel :: KindSel
  , only :: [Text]
  , skip :: [Text]
  , category :: Maybe Text
  }

-- | 1サイト・1検索語・1種類ぶんの仕事
data Job = Job
  { def :: SiteDef
  , query :: Text
  , kind :: Kind
  , -- | 検索URL。組み立てられないサイトは 'Nothing'
    url :: Maybe Text
  , -- | 取りに行かずに決まる結果（対象外・種類が合わない など）
    fixed :: Maybe Status
  }

-- | 検索語を、サイトの文字コードで % エンコードして、ひな形に入れる
searchUrl :: Enc -> Text -> Text -> Text
searchUrl enc template q = T.replace "{q}" (percentEncode (encodeQuery enc q)) template

-- | カタログから仕事の一覧を作る（純粋）。全サイトについて、少なくとも1行ずつ出る。
plan :: SearchOpts -> [Job]
plan o = concatMap siteJobs (filter wanted catalog)
 where
  wanted s =
    (null o.only || s.sid `elem` o.only)
      && s.sid `notElem` o.skip
      && maybe True (\c -> c `T.isInfixOf` categoryName s.category) o.category
  siteJobs s = concatMap (jobsFor s) o.queries
  jobsFor s q =
    let mk k t = Job s q k (searchUrl s.enc <$> t <*> Just q) Nothing
        new = mk New s.searchNew
        used = mk Used s.searchUsed
        defaultKind = if KNew `elem` s.kinds then New else Used
        none = Job s q defaultKind Nothing Nothing
        mismatch k why = Job s q k Nothing (Just (OutOfScope why))
     in case (o.kindSel, s.searchNew, s.searchUsed) of
          _ | s.category == NonPrice -> [mismatch defaultKind "価格以外の情報源（meguri recall で調べる）"]
          (_, Nothing, Nothing) -> [none]
          (SelAll, Just _, Just _) -> [new, used]
          (SelAll, Just _, Nothing) -> [new]
          (SelAll, Nothing, Just _) -> [used]
          (SelNew, Just _, _) -> [new]
          (SelNew, Nothing, _) -> [mismatch New "中古だけを扱う（--kind new では当たらない）"]
          (SelUsed, _, Just _) -> [used]
          (SelUsed, Just _, Nothing)
            -- 中古も扱うが、中古だけに絞るURLが無いサイトは、区別なしの検索を使う
            | any (`elem` s.kinds) [KUsed, KAuction, KP2P] -> [mk Used s.searchNew]
            | otherwise -> [mismatch Used "新品だけを扱う（--kind used では当たらない）"]

runSearch :: Fetcher -> SearchOpts -> IO ([Coverage], [Candidate])
runSearch f o = runJobs f (plan o)

runJobs :: Fetcher -> [Job] -> IO ([Coverage], [Candidate])
runJobs f jobs = do
  blocked <- newTVarIO Set.empty
  results <- mapConcurrently (runJob f blocked) jobs
  pure (map fst results, concatMap snd results)

runJob :: Fetcher -> TVar (Set Text) -> Job -> IO (Coverage, [Candidate])
runJob f blocked j = do
  now <- jstStamp <$> getCurrentTime
  let covAt at status method = Coverage {site = j.def.sid, query = j.query, kind = j.kind, status, url = j.url, method, at}
      cov = covAt now
      hint = fromMaybe "" (head j.def.hints)
      listX = (adapterFor j.def).listX
      entry = fromMaybe "検索URLなし" j.def.searchNote
  case (j.fixed, j.def.tier, j.url, listX) of
    (Just st, _, _, _) -> pure (cov st "カタログ", [])
    (_, TierOutOfScope, _, _) -> pure (cov (OutOfScope hint) "カタログ", [])
    (_, TierStopped why, _, _) -> pure (cov (Stopped (show j.def.checked) why) "カタログ（日を改めて開き直す）", [])
    (_, TierBlocked, _, _) -> pure (cov (Blocked hint) "カタログ", [])
    (_, Http, Just u, Just ex) -> do
      already <- Set.member (hostOf u) <$> readTVarIO blocked
      if already
        then pure (cov (Blocked "同じホストが先の取得でブロックされたので、やめた") "HTTP", [])
        else fetchList f blocked j u ex covAt
    (_, Http, Just _, Nothing) -> pure (cov (NeedsBrowser "HTTPで取れるが、アダプタがまだ無い") "ブラウザで開く", [])
    (_, Http, Nothing, _) -> pure (cov (NeedsBrowser entry) "ブラウザで開く", [])
    (_, HttpItem, _, _) -> pure (cov (NeedsBrowser (maybe "検索はJS描画。個別ページはHTTPで読める（meguri item）" (const entry) j.def.searchNote)) "ブラウザで開く", [])
    (_, BrowserJs, _, _) -> pure (cov (NeedsBrowser (maybe "JSの描画が要る（ヘッドレスでよい）" (const entry) j.def.searchNote)) "ブラウザで開く", [])
    (_, BrowserNormal, _, _) -> pure (cov (NeedsBrowser (maybe "画面ありの通常のブラウザが要る" (const entry) j.def.searchNote)) "通常のブラウザで開く", [])

fetchList
  :: Fetcher
  -> TVar (Set Text)
  -> Job
  -> Text
  -> (ListCtx -> Tags -> Text -> ListOut)
  -> (Text -> Status -> Text -> Coverage)
  -> IO (Coverage, [Candidate])
fetchList f blocked j u ex covAt = do
  first <- one u
  case first of
    Left st -> do
      now <- jstStamp <$> getCurrentTime
      pure (covAt now st "HTTP", [])
    -- 確認時刻は、ページを取った時刻（キャッシュなら、もとの取得の時刻）
    Right (out, (method, fetched)) -> do
      -- 続きのページ（価格.com のショップ一覧など）。失敗しても、取れた分は残す
      more <- followAll (4 :: Int) out.follow
      let offers = [tidyOffer x | x <- out.offers <> concatMap (.offers) more, usableUrl x.url]
          cands = [Candidate {query = j.query, kind = j.kind, offer = x} | x <- offers]
          cheapest = minimumMay (mapMaybe (\x -> x.priceIncl.value) offers)
          status
            | not (null offers) = Found (length offers) cheapest
            | out.zero = Zero
            | otherwise = Error "一覧から1件も抜き出せなかった（ページの作りが変わったかもしれない）"
      pure (covAt fetched status method, cands)
 where
  -- 続きのページは、1つの仕事につき4ページまで
  followAll _ [] = pure []
  followAll budget (t : ts)
    | budget <= 0 = pure []
    | otherwise = do
        r <- one t
        case r of
          Right (o, _) -> (o :) <$> followAll (budget - 1) (ts <> o.follow)
          Left _ -> followAll (budget - 1) ts
  one target = do
    r <- fetchPage f target
    case r of
      Left e -> pure (Left (Error e))
      Right p -> do
        let (_, text) = decodeBody p.contentType p.body
            ctx = ListCtx {siteId = j.def.sid, query = j.query, kind = j.kind, pageUrl = target, httpStatus = p.status, stamp = jstStamp p.fetchedAt}
            out = ex ctx (parse text) text
            method = (if p.cached then "HTTP（キャッシュ）" else "HTTP" :: Text, jstStamp p.fetchedAt)
        case classify p.status text of
          PageBlocked why -> do
            atomically (modifyTVar' blocked (Set.insert (hostOf target)))
            pure (Left (Blocked why))
          PageStopped why -> pure (Left (Stopped (T.take 10 (jstStamp p.fetchedAt)) why))
          PageError why -> pure (Left (Error why))
          PageNotFound
            | out.zero -> pure (Right (out, method))
            | otherwise -> pure (Left (Error "HTTP 404"))
          PageOk -> pure (Right (out, method))
