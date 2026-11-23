-- | 検索結果の一覧：全サイトを並列に当たり、候補（一覧の値）と「どこまで探したか」を作る。
-- 個別ページの確認まで含めた search の全体は "Meguri.Search"
module Meguri.List
  ( Lookup (..)
  , Wider (..)
  , widen
  , widenedNote
  , hasAll
  , lookups
  , searchUrl
  , listAll
  , listThen
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVarIO)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Meguri.Catalog
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Meguri.Extract.Html (Tags, parse)
import Meguri.Fetch.Encoding
import Meguri.Fetch.Http
import Meguri.Sanitize (tidyOffer, usableUrl)
import Meguri.Site.Common
import Meguri.Sites
import Meguri.Text (contains, jstStamp, percentEncode, toHalfWidth)
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
  , -- | 語を広げた検索も当たるとき、その検索
    wider :: Maybe Wider
  }

-- | 語を広げた検索：広げた語の検索URLと、その結果から残すものの条件（題に、この語を全部含む）。
-- 比べ方は @--has@ と同じ（大文字小文字・全角半角・空白・ハイフンの違いは無視）
data Wider = Wider
  { url :: Text
  , has :: [Text]
  }

-- | 検索語を広げる。英字と数字の切れ目で語を切り、語ごとに一致を見る検索（ヤフオク・Yahoo!フリマ）では、
-- 英字で終わる型番（NA-LX127C）で探すと、英字に続きのある題（NA-LX127CL・NA-LX127CR）が出ない。
--
-- 数字のあとの末尾の英字を外した検索語（NA-LX127）と、その結果から残すものが題に含むべき、もとの語（NA-LX127C）を返す。
-- そういう語が無ければ（WH-1000XM5・ヘッドホン など）'Nothing'。
widen :: Text -> Maybe (Text, [Text])
widen q =
  let cuts = [(w, cut w) | w <- T.words q]
   in if any (isJust . snd) cuts
        then Just (T.unwords [fromMaybe w c | (w, c) <- cuts], [w | (w, Just _) <- cuts])
        else Nothing
 where
  cut w =
    let half = toHalfWidth w
        front = T.dropWhileEnd letter half
     in if T.length front < T.length half && not (T.null front) && isDigit (T.last front) then Just front else Nothing
  letter c = isAsciiUpper c || isAsciiLower c

-- | 語を広げて探したことの説明。広げた語ごとに1行（表の下に出す）
widenedNote :: [Lookup] -> Text
widenedNote js =
  T.unlines
    [ "「" <> q <> "」は、" <> T.intercalate "・" sids <> " で「" <> w <> "」でも探し、題に" <> T.intercalate "・" ["「" <> h <> "」" | h <- has] <> "を含むものを足した"
    | q <- ordNub [j.term | j <- js, isJust j.wider]
    , Just (w, has) <- [widen q]
    , let sids = ordNub [j.channel.sid | j <- js, j.term == q, isJust j.wider]
    ]

-- | 検索語を、サイトの文字コードで % エンコードして、ひな形に入れる
searchUrl :: Enc -> Text -> Text -> Text
searchUrl enc template q = T.replace "{q}" (percentEncode (encodeQuery enc q)) template

-- | 題に語を全部含むか（比べ方は @--has@ と同じ）
hasAll :: [Text] -> Offer m -> Bool
hasAll ws x = maybe False (\n -> all (`contains` n) ws) x.name.value

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
    let mk k t = Lookup s q k (searchUrl s.enc <$> t <*> Just q) Nothing (wider t)
        -- 英字で終わる型番は、英字を外した語でも、同じひな形で探す（検索が英字と数字の切れ目で語を切るサイトだけ）
        wider t = do
          guard s.widenCodes
          template <- t
          (w, has) <- widen q
          pure (Wider (searchUrl s.enc template w) has)
        new = mk New s.searchNew
        used = mk Used s.searchUsed
        defaultKind = if KNew `elem` s.kinds then New else Used
        none = Lookup s q defaultKind Nothing Nothing Nothing
        mismatch k why = Lookup s q k Nothing (Just (OutOfScope why)) Nothing
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
  blocked <- newTVarIO Map.empty
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
        pure (Coverage {site = j.channel.sid, term = j.term, market = j.market, status = Error "時間切れ（--time で延ばせる）", url = j.url, method = "HTTP", at = now}, [], Nothing)
  within act = case deadline of
    Nothing -> Just <$> act
    Just d -> do
      now <- getCurrentTime
      let left = realToFrac (diffUTCTime d now) :: Double
      if left <= 0 then pure Nothing else timeout (ceiling (left * 1000000)) act

runJob :: Fetcher -> TVar (Map Text Block) -> Lookup -> IO (Coverage, [Sighting])
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
    (_, TierBlocked, _, _) -> pure (cov (BlockedBefore (show j.channel.checked) hint) "カタログ", [])
    (_, Http, Just u, Just ex) -> do
      already <- Map.lookup (hostOf u) <$> readTVarIO blocked
      case already of
        Just b -> pure (cov (Blocked b) "HTTP", [])
        Nothing -> fetchList f blocked j u ex covAt
    (_, Http, Just _, Nothing) -> pure (cov (NeedsBrowser "HTTPで取れるが、アダプタがまだ無い") "ブラウザで開く", [])
    (_, Http, Nothing, _) -> pure (cov (NeedsBrowser entry) "ブラウザで開く", [])
    (_, HttpItem, _, _) -> pure (cov (NeedsBrowser (maybe "検索はJS描画。個別ページはHTTPで読める（見つけた商品のURLを --url で渡す）" (const entry) j.channel.searchNote)) "ブラウザで開く", [])
    (_, BrowserJs, _, _) -> pure (cov (NeedsBrowser (maybe "JSの描画が要る（ヘッドレスでよい）" (const entry) j.channel.searchNote)) "ブラウザで開く", [])
    (_, BrowserNormal, _, _) -> pure (cov (NeedsBrowser (maybe "画面ありの通常のブラウザが要る" (const entry) j.channel.searchNote)) "通常のブラウザで開く", [])

fetchList
  :: Fetcher
  -> TVar (Map Text Block)
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
    -- 確認時刻は、ページを取った時刻
    Right (out, (method, fetched)) -> do
      -- 続きのページ（価格.com のショップ一覧など）。失敗しても、取れた分は残す
      more <- followAll (4 :: Int) out.follow
      -- 語を広げた検索。題に語を含むもののうち、まだ無いものを足す。失敗しても、取れた分は残す
      wide <- maybe (pure []) widerOffers j.wider
      let found = out.offers <> concatMap (.offers) more
          added = [x | x <- wide, x.url `notElem` map (.url) found]
          offers = [tidyOffer x | x <- found <> added, usableUrl x.url]
          cands = [Sighting {term = j.term, market = j.market, offer = x} | x <- offers]
          cheapest = minimumMay (mapMaybe (\x -> x.priceIncl.value) offers)
          status
            | not (null offers) = Found (length offers) cheapest
            | out.zero = Zero
            | otherwise = Error "一覧から1件も抜き出せなかった（ページの作りが変わったかもしれない）"
      pure (covAt fetched status method, cands)
 where
  widerOffers w = do
    r <- one w.url
    pure $ case r of
      Right (o, _) -> filter (hasAll w.has) o.offers
      Left _ -> []
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
      Left (FetchBlocked b) -> do
        atomically (modifyTVar' blocked (Map.insert (hostOf target) b))
        pure (Left (Blocked b))
      Left (FetchFailed e) -> pure (Left (Error e))
      Right p -> do
        let (_, text) = decodeBody p.contentType p.body
            ctx = ListCtx {siteId = j.channel.sid, term = j.term, market = j.market, pageUrl = target, httpStatus = p.status, stamp = jstStamp p.fetchedAt}
            out = ex ctx (parse text) text
            method = ("HTTP" :: Text, jstStamp p.fetchedAt)
        case classify p.status text of
          PageStopped why -> pure (Left (Stopped (T.take 10 (jstStamp p.fetchedAt)) why))
          PageError why -> pure (Left (Error why))
          PageNotFound
            | out.zero -> pure (Right (out, method))
            | otherwise -> pure (Left (Error "HTTP 404"))
          PageOk -> pure (Right (out, method))
