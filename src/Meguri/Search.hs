-- | search の中身。形は「何を（'Query'）→ どこを（'Scope'）→ 結果（'Report'）」。'Effort' は、どこまでやるか。
--
-- @
-- scope.channels ─ カタログから選ぶ ─→ 当たる販路 ─┐
-- scope.markets ─────────────────────────────────┤
-- query.terms ───────────────────────────────────┴ lookups ─→ list ─→ どこまで探したか・一覧で見かけた売り物
-- query.accept ─ 見かけた売り物から選ぶ ─ cheapest ─→ 読むページ ─┐
-- scope.pages（名指し） ─────────────────────────────────────────┴ readPages ─→ 読めたもの・読めなかったもの
-- query.accept の条件 ─ 読めた値でもう一度 ─→ arrange ─→ Report
-- @
--
-- 結果は、'Request' だけで決まる。前に走らせた結果は使わない。
-- 取ったページと判定の答えは15分キャッシュされるので、同じ頼みをもう一度走らせても、サイトには当たらない。
module Meguri.Search
  ( Request (..)
  , Effort (..)
  , Report (..)
  , search
  , assemble
  ) where

import Data.List (nubBy, partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (addUTCTime, getCurrentTime)
import Meguri.Arrange (Finding (..), arrange)
import Meguri.Catalog (Channel)
import Meguri.Fetch.Http (Fetcher)
import Meguri.List (listThen, lookups)
import Meguri.Query
import Meguri.Read
import Meguri.Scope (Scope (..), channelsIn)
import Meguri.Selection
import Meguri.Ship (Pref)
import Meguri.Types
import Protolude

data Request = Request
  { -- | 何を探すか
    query :: Query
  , -- | どこを探すか
    scope :: Scope
  , -- | どこまでやるか
    effort :: Effort
  }

data Effort = Effort
  { -- | 販路と新品・中古の組ごとに、安い順で何件の個別ページを読むか（0 なら一覧だけ）
    readsPerLookup :: Int
  , -- | 全体にかける時間の上限（秒）。過ぎたら、読めた分だけで返す
    within :: Int
  }

data Report = Report
  { -- | 当たった販路と、選び方が外した数
    channels :: Selected Channel
  , -- | どこまで探したか（検索1回につき1行）
    coverage :: [Coverage]
  , -- | 一覧で見かけた売り物の全部
    sightings :: [Sighting]
  , -- | そのうち、探している物と認めたものと、選び方が外した数
    accepted :: Selected Sighting
  , -- | 個別ページを読めたもの
    checked :: [Checked]
  , -- | 個別ページを読めなかったもの
    unread :: [Unread]
  , -- | 個別ページの値で、条件が外した数
    rechecked :: [Excluded]
  , -- | 重複を除いて、枠に分けたもの。個別ページを読んだものが先
    findings :: [Finding]
  }

-- | 判定（外のコマンド・jev）が失敗したら 'Left'（かけたことにして先へ進まない）。
--
-- 判定が無ければ、一覧が取れた販路から順に、条件をかけて個別ページを読み始める（遅い販路の一覧を待たない）。
-- 判定があるときは、全部の一覧がそろってから、まとめてかける。
search :: Ask -> Fetcher -> Maybe Pref -> Request -> IO (Either Text Report)
search ask f pref req = do
  chosen <- channelsIn ask req.query.terms req.scope
  case chosen of
    Left e -> pure (Left e)
    Right channels -> do
      start <- getCurrentTime
      let deadline = Just (addUTCTime (fromIntegral req.effort.within) start)
          todo = lookups req.query.terms req.scope.markets channels.chosen
          accept = req.query.accept
          readNamed = readPages f pref deadline req.scope.pages
          readCheapest = readPages f pref deadline . cheapest req.effort.readsPerLookup
          done accepted reads coverage sightings = Right (assemble accept channels accepted reads coverage sightings)
      if null accept.judges
        then do
          let perLookup seen = do
                let accepted = sift accept.criteria ofSighting seen
                reads <- readCheapest accepted.chosen
                pure (accepted, reads)
          ((coverage, sightings, each), fromNamed) <- concurrently (listThen f deadline todo perLookup) readNamed
          let accepted =
                Selected
                  (concatMap (\(a, _) -> a.chosen) each)
                  (foldl' addExcluded [Excluded l 0 | l <- labels accept] (map (\(a, _) -> a.excluded) each))
          pure (done accepted (fromNamed <> mconcat (map snd each)) coverage sightings)
        else do
          ((coverage, sightings, _), fromNamed) <- concurrently (listThen f deadline todo (const (pure ()))) readNamed
          selected <- select ask seenRows accept ofSighting sightings
          case selected of
            Left e -> pure (Left e)
            Right accepted -> do
              reads <- readCheapest accepted.chosen
              pure (done accepted (fromNamed <> reads) coverage sightings)

-- | 取ったものを、結果にまとめる（純粋）
assemble :: Selection Seen -> Selected Channel -> Selected Sighting -> ([Checked], [Unread]) -> [Coverage] -> [Sighting] -> Report
assemble accept channels accepted (checked0, unread0) coverage sightings =
  let -- 同じページを2回選んでいたら（検索語が複数のとき）、1つにする。名指しのページが先
      once :: (a -> Page) -> [a] -> [a]
      once page = nubBy ((==) `on` (\x -> (page x).url))
      checked = once (.page) checked0
      unread = [u | u <- once (.page) unread0, u.page.url `notElem` map (\c -> c.page.url) checked]
      (fromNamed, fromPicked) = partition (\c -> named c.page) checked
      -- 個別ページの値で、条件をもう一度かける（一覧と個別ページで価格が違うことがある）
      again = sift accept.criteria (\c -> ofReading c.page.sighting c.reading) fromPicked
      shown = fromNamed <> again.chosen
      drifts = Map.fromList [(c.reading.offer.url, c.drift) | c <- shown]
      readUrls = Set.fromList (map (\c -> c.page.url) checked)
      -- 読めなかったもの（ブラウザが要る・失敗）は、一覧の値のまま残す
      leftover = [s | s <- accepted.chosen, s.offer.url `Set.notMember` readUrls]
      findings = [e {notes = Map.findWithDefault [] e.url drifts <> e.notes} | e <- arrange (map (.reading) shown) leftover]
   in Report {channels, coverage, sightings, accepted, checked, unread, rechecked = again.excluded, findings}
