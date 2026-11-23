-- | 検索結果の一覧：全サイトを並列に当たり、候補（一覧の値）と「どこまで探したか」を作る。
-- 個別ページの確認まで含めた search の全体は "Meguri.Search"
module Meguri.List
  ( Lookup (..)
  , Wider (..)
  , widen
  , hasAll
  , lookups
  , searchUrl
  , listAll
  , listThen
  , settle
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVarIO)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Meguri.Catalog
import Data.Time (UTCTime, getCurrentTime)
import Meguri.Extract.Html (Tags, parse)
import Meguri.Fetch.Encoding
import Meguri.Fetch.Http
import Meguri.Sanitize (tidyOffer, usableUrl)
import Meguri.Site.Common
import Meguri.Sites
import Meguri.Text (contains, jstStamp, percentEncode, toHalfWidth)
import Meguri.Types
import Protolude

-- | 検索1回：1つの販路に、1つの語を、新品か中古のどちらかで当てる
data Lookup = Lookup
  { channel :: Channel
  , term :: Text
  , market :: Market
  , -- | 検索URL。組み立てられないサイトは 'Nothing'
    url :: Maybe Text
  , -- | 取りに行かないと、初めから決まっているときの理由（対象外・種類が合わない など）
    fixed :: Maybe Skip
  , -- | 語を広げた検索も当たるとき、その検索
    wider :: Maybe Wider
  }

-- | 語を広げた検索：広げた語と、その検索URLと、結果から残すものの条件（題に、この語を全部含む）。
-- 比べ方は @--has@ と同じ（大文字小文字・全角半角・空白・ハイフンの違いは無視）
data Wider = Wider
  { term :: Text
  , url :: Text
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
          pure (Wider w (searchUrl s.enc template w) has)
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
-- 期限を過ぎたら、取りに行くのをそこでやめる。それまでに取れた分は残り、やめた取得は「時間切れ」と残る。
listThen :: Fetcher -> Maybe UTCTime -> [Lookup] -> ([Sighting] -> IO a) -> IO ([Coverage], [Sighting], [a])
listThen f deadline jobs next = do
  blocked <- newTVarIO Map.empty
  results <- mapConcurrently (one blocked) jobs
  pure (map (\(c, _, _) -> c) results, concatMap (\(_, cs, _) -> cs) results, map (\(_, _, a) -> a) results)
 where
  one blocked j = do
    (cov, cands) <- runJob f deadline blocked j
    a <- next cands
    pure (cov, cands, a)

-- | 検索1回。取りに行かない販路は、その理由を返す。取りに行く販路は、取得の並びと、残した売り物を返す
runJob :: Fetcher -> Maybe UTCTime -> TVar (Map Text Block) -> Lookup -> IO (Coverage, [Sighting])
runJob f deadline blocked j = do
  now <- jstStamp <$> getCurrentTime
  let row at did cheapest method = Coverage {site = j.channel.sid, term = j.term, market = j.market, url = j.url, did, cheapest, method, at}
      skip why method = pure (row now (Skipped why) Nothing method, [])
      hint = fromMaybe "" (head j.channel.hints)
      listX = (adapterFor j.channel).listX
      entry = fromMaybe "検索URLなし" j.channel.searchNote
  case (j.fixed, j.channel.tier, j.url, listX) of
    (Just why, _, _, _) -> skip why "カタログ"
    (_, TierOutOfScope, _, _) -> skip (OutOfScope hint) "カタログ"
    (_, TierStopped why, _, _) -> skip (StoppedBefore (show j.channel.checked) why) "カタログ（日を改めて開き直す）"
    (_, TierBlocked, _, _) -> skip (BlockedBefore (show j.channel.checked) hint) "カタログ"
    (_, Http, Just u, Just ex) -> do
      (as, kept) <- fetchList f deadline blocked j u ex
      let cands = [Sighting {term = j.term, market = j.market, offer = x, via} | (via, x) <- kept]
          cheapest = minimumMay (mapMaybe (\(_, x) -> x.priceIncl.value) kept)
      -- 確認時刻は、最初のページを取った時刻
      pure (row (maybe now (.at) (head as)) (Tried as) cheapest "HTTP", cands)
    (_, Http, Just _, Nothing) -> skip (NeedsBrowser "HTTPで取れるが、アダプタがまだ無い") "ブラウザで開く"
    (_, Http, Nothing, _) -> skip (NeedsBrowser entry) "ブラウザで開く"
    (_, HttpItem, _, _) -> skip (NeedsBrowser (maybe "検索はJS描画。個別ページはHTTPで読める（見つけた商品のURLを --url で渡す）" (const entry) j.channel.searchNote)) "ブラウザで開く"
    (_, BrowserJs, _, _) -> skip (NeedsBrowser (maybe "JSの描画が要る（ヘッドレスでよい）" (const entry) j.channel.searchNote)) "ブラウザで開く"
    (_, BrowserNormal, _, _) -> skip (NeedsBrowser (maybe "画面ありの通常のブラウザが要る" (const entry) j.channel.searchNote)) "通常のブラウザで開く"

-- | 検索1回で取るページを、順に取りに行く：1ページ目、続きのページ、語を広げた検索。
-- 取得の並び（した順）と、残した売り物（見かけたページつき）を返す。
--
-- 1ページ目が取れなければ、そこでやめる。続きのページと語を広げた検索は、取れなくても、ほかで取れた分を残す。
fetchList
  :: Fetcher
  -> Maybe UTCTime
  -> TVar (Map Text Block)
  -> Lookup
  -> Text
  -> (ListCtx -> Tags -> Text -> ListOut)
  -> IO ([Attempt], [(Via, Offer OnList)])
fetchList f deadline blocked j u ex = do
  already <- Map.lookup (hostOf u) <$> readTVarIO blocked
  case already of
    -- 同じホストが、この実行でもうブロックされていたら、取りに行かない
    Just b -> do
      now <- jstStamp <$> getCurrentTime
      pure ([Attempt {role = SearchPage, url = u, at = now, outcome = HostBlocked b, listed = Nothing}], [])
    Nothing -> do
      first <- load SearchPage u
      case first of
        (a, Nothing) -> pure ([a], [])
        (_, Just out) -> do
          -- 続きのページ（価格.com のショップ一覧など）
          more <- followAll (4 :: Int) out.follow
          -- 語を広げた検索。時間切れのあとは、取りに行かない
          wide <-
            if any (\(a, _) -> a.outcome == TimedOut) more
              then pure []
              else traverse (\w -> load (WiderPage w.term w.has) w.url) (maybeToList j.wider)
          pure (settle (first : more <> wide))
 where
  -- 続きのページは、1つの検索につき4ページまで
  followAll _ [] = pure []
  followAll budget (t : ts)
    | budget <= 0 = pure []
    | otherwise = do
        r@(a, out) <- load FollowPage t
        if a.outcome == TimedOut
          then pure [r]
          else (r :) <$> followAll (budget - 1) (ts <> maybe [] (.follow) out)
  -- 1ページを取りに行って、一覧を読む。ブロックされたら、そのホストを覚える
  load role target = do
    (a, loaded) <- attempt f deadline role (page target)
    case a.outcome of
      Blocked b -> atomically (modifyTVar' blocked (Map.insert (hostOf target) b))
      _ -> pure ()
    pure . (a,) $ do
      p <- loaded
      let ctx = ListCtx {siteId = j.channel.sid, term = j.term, market = j.market, pageUrl = target, httpStatus = p.status, stamp = p.at}
          out = ex ctx (parse p.text) p.text
      -- 404 のページは、サイトが「該当なし」と返したときだけ読む
      guard (a.outcome == Got || out.zero)
      pure out

-- | 取れたページから、残す売り物を決める（純粋）。取得には、読めた数と残した数を書き込む。
--
-- URLの形が正しくないものは残さない。語を広げた検索からは、題にもとの語を全部含み、まだ出ていないものだけを残す。
settle :: [(Attempt, Maybe ListOut)] -> ([Attempt], [(Via, Offer OnList)])
settle pages = (map fst done, concatMap snd done)
 where
  done = go [] pages
  go _ [] = []
  go seen ((a, read) : rest) = case read of
    Nothing -> (a, []) : go seen rest
    Just out ->
      let keep x =
            usableUrl x.url && case a.role of
              WiderPage _ has -> hasAll has x && x.url `notElem` seen
              _ -> True
          kept = [tidyOffer x | x <- out.offers, keep x]
          listed = Just Listed {offers = length out.offers, zero = out.zero, kept = length kept}
       in (a {listed}, [(Via a.role a.url, x) | x <- kept]) : go (seen <> map (.url) out.offers) rest
