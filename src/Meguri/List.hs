-- | 検索結果の一覧：全サイトを並列に当たり、候補（一覧の値）と「どこまで探したか」を作る。
-- 個別ページの確認まで含めた search の全体は "Meguri.Search"
module Meguri.List
  ( Lookup (..)
  , lookups
  , searchUrl
  , listAll
  , listThen
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVarIO)
import Data.Set qualified as Set
import Data.Text qualified as T
import Meguri.Catalog
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Meguri.Extract.Html (Tags, parse)
import Meguri.Fetch.Encoding
import Meguri.Fetch.Http
import Meguri.Sanitize (tidyOffer, usableUrl)
import Meguri.Site.Common
import Meguri.Sites
import Meguri.Text (jstStamp, percentEncode)
import Meguri.Types
import Protolude
import System.Timeout (timeout)

-- | 検索1回：1つの販路に、1つの語を、新品か中古のどちらかで当てる
data Lookup = Lookup
  { channel :: Channel
  , term :: Text
  , market :: Market
  , -- | 検索URL。組み立てられないサイトは 'Nothing'
    url :: Maybe Text
  , -- | 取りに行かずに決まる結果（対象外・種類が合わない など）
    fixed :: Maybe Status
  }

-- | 検索語を、サイトの文字コードで % エンコードして、ひな形に入れる
searchUrl :: Enc -> Text -> Text -> Text
searchUrl enc template q = T.replace "{q}" (percentEncode (encodeQuery enc q)) template

data MarketSel = SelNew | SelUsed | SelAll

-- | 検索語・新品か中古か・当たる販路から、検索の一覧を作る（純粋）。渡した販路の全部について、少なくとも1つずつ出る。
-- 販路は "Meguri.Scope" で選んだもの。新品・中古は、空なら両方。
lookups :: [Text] -> [Market] -> [Channel] -> [Lookup]
lookups terms markets = concatMap siteJobs
 where
  sel = case ordNub markets of
    [New] -> SelNew
    [Used] -> SelUsed
    _ -> SelAll
  siteJobs s = concatMap (jobsFor s) terms
  jobsFor s q =
    let mk k t = Lookup s q k (searchUrl s.enc <$> t <*> Just q) Nothing
        new = mk New s.searchNew
        used = mk Used s.searchUsed
        defaultKind = if KNew `elem` s.kinds then New else Used
        none = Lookup s q defaultKind Nothing Nothing
        mismatch k why = Lookup s q k Nothing (Just (OutOfScope why))
     in case (sel, s.searchNew, s.searchUsed) of
          _ | s.category == NonPrice -> [mismatch defaultKind "価格以外の情報源（meguri recall で調べる）"]
          (_, Nothing, Nothing) -> [none]
          (SelAll, Just _, Just _) -> [new, used]
          (SelAll, Just _, Nothing) -> [new]
          (SelAll, Nothing, Just _) -> [used]
          (SelNew, Just _, _) -> [new]
          (SelNew, Nothing, _) -> [mismatch New "中古だけを扱う（--market new では当たらない）"]
          (SelUsed, _, Just _) -> [used]
          (SelUsed, Just _, Nothing)
            -- 中古も扱うが、中古だけに絞るURLが無いサイトは、区別なしの検索を使う
            | any (`elem` s.kinds) [KUsed, KAuction, KP2P] -> [mk Used s.searchNew]
            | otherwise -> [mismatch Used "新品だけを扱う（--market used では当たらない）"]

listAll :: Fetcher -> [Lookup] -> IO ([Coverage], [Sighting])
listAll f jobs = do
  (cov, cands, _) <- listThen f Nothing jobs (const (pure ()))
  pure (cov, cands)

-- | 仕事を並列に走らせ、一覧が取れた仕事から順に、その候補で続きの作業（個別ページの確認など）をする。
-- 期限を過ぎた仕事は、そこでやめて「時間切れ」と記録する。
listThen :: Fetcher -> Maybe UTCTime -> [Lookup] -> ([Sighting] -> IO a) -> IO ([Coverage], [Sighting], [a])
listThen f deadline jobs next = do
  blocked <- newTVarIO Set.empty
  results <- mapConcurrently (one blocked) jobs
  pure (map (\(c, _, _) -> c) results, concatMap (\(_, cs, _) -> cs) results, mapMaybe (\(_, _, a) -> a) results)
 where
  one blocked j = do
    listed <- within (runJob f blocked j)
    case listed of
      Just (cov, cands) -> do
        a <- next cands
        pure (cov, cands, Just a)
      Nothing -> do
        now <- jstStamp <$> getCurrentTime
        pure (Coverage {site = j.channel.sid, term = j.term, market = j.market, status = Error "時間切れ（--time を延ばすか、もう一度走らせる）", url = j.url, method = "HTTP", at = now}, [], Nothing)
  within act = case deadline of
    Nothing -> Just <$> act
    Just d -> do
      now <- getCurrentTime
      let left = realToFrac (diffUTCTime d now) :: Double
      if left <= 0 then pure Nothing else timeout (ceiling (left * 1000000)) act

runJob :: Fetcher -> TVar (Set Text) -> Lookup -> IO (Coverage, [Sighting])
runJob f blocked j = do
  now <- jstStamp <$> getCurrentTime
  let covAt at status method = Coverage {site = j.channel.sid, term = j.term, market = j.market, status, url = j.url, method, at}
      cov = covAt now
      hint = fromMaybe "" (head j.channel.hints)
      listX = (adapterFor j.channel).listX
      entry = fromMaybe "検索URLなし" j.channel.searchNote
  case (j.fixed, j.channel.tier, j.url, listX) of
    (Just st, _, _, _) -> pure (cov st "カタログ", [])
    (_, TierOutOfScope, _, _) -> pure (cov (OutOfScope hint) "カタログ", [])
    (_, TierStopped why, _, _) -> pure (cov (Stopped (show j.channel.checked) why) "カタログ（日を改めて開き直す）", [])
    (_, TierBlocked, _, _) -> pure (cov (Blocked hint) "カタログ", [])
    (_, Http, Just u, Just ex) -> do
      already <- Set.member (hostOf u) <$> readTVarIO blocked
      if already
        then pure (cov (Blocked "同じホストが先の取得でブロックされたので、やめた") "HTTP", [])
        else fetchList f blocked j u ex covAt
    (_, Http, Just _, Nothing) -> pure (cov (NeedsBrowser "HTTPで取れるが、アダプタがまだ無い") "ブラウザで開く", [])
    (_, Http, Nothing, _) -> pure (cov (NeedsBrowser entry) "ブラウザで開く", [])
    (_, HttpItem, _, _) -> pure (cov (NeedsBrowser (maybe "検索はJS描画。個別ページはHTTPで読める（見つけた商品のURLを --url で渡す）" (const entry) j.channel.searchNote)) "ブラウザで開く", [])
    (_, BrowserJs, _, _) -> pure (cov (NeedsBrowser (maybe "JSの描画が要る（ヘッドレスでよい）" (const entry) j.channel.searchNote)) "ブラウザで開く", [])
    (_, BrowserNormal, _, _) -> pure (cov (NeedsBrowser (maybe "画面ありの通常のブラウザが要る" (const entry) j.channel.searchNote)) "通常のブラウザで開く", [])

fetchList
  :: Fetcher
  -> TVar (Set Text)
  -> Lookup
  -> Text
  -> (ListCtx -> Tags -> Text -> ListOut)
  -> (Text -> Status -> Text -> Coverage)
  -> IO (Coverage, [Sighting])
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
          cands = [Sighting {term = j.term, market = j.market, offer = x} | x <- offers]
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
            ctx = ListCtx {siteId = j.channel.sid, term = j.term, market = j.market, pageUrl = target, httpStatus = p.status, stamp = jstStamp p.fetchedAt}
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
