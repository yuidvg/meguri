-- | verify：候補の個別ページを取り直して、確認済みかどうかを決め、一覧や前回との違いを出す
module Meguri.Verify
  ( Target (..)
  , Outcome (..)
  , Row (..)
  , targets
  , verifyAll
  , changesFromListing
  , changesBetween
  , report
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Meguri.Fetch.Http (Fetcher)
import Meguri.Item
import Meguri.Rank (latestItems)
import Meguri.Ship (Pref)
import Meguri.Render (confirmationMark, rankCandidates, shippingShort, stockShort, yen)
import Meguri.Text (clean)
import Meguri.Types
import Protolude

-- | 読み直す対象
data Target = Target
  { url :: Text
  , -- | 一覧で見た値
    listed :: Maybe Candidate
  , -- | 前に個別ページを読んだときの記録
    previous :: Maybe ItemRecord
  }

data Outcome
  = Read ItemRecord
  | -- | HTTP では読めない。ブラウザで開いて、保存したHTMLを item --html で渡す
    NeedsHtml Text
  | Failed Text

data Row = Row
  { target :: Target
  , outcome :: Outcome
  , changes :: [Text]
  }

-- | 前に読んだ個別ページすべてと、候補の上位 N 件（安い順。同じ在庫は1つ）
targets :: Int -> [ItemRecord] -> [Candidate] -> [Target]
targets top items cands =
  let known = Map.fromList [(r.offer.url, r) | r <- latestItems items]
      listedByUrl = Map.fromList [(c.offer.url, c) | c <- reverse cands]
      again = [Target u (Map.lookup u listedByUrl) (Just r) | (u, r) <- Map.toList known]
      fresh = [Target c.offer.url (Just c) Nothing | c <- take top (filter (\c -> not (Map.member c.offer.url known)) (rankCandidates cands))]
   in again <> fresh

verifyAll :: Fetcher -> Maybe Pref -> [Target] -> IO [Row]
verifyAll f pref = mapConcurrently one
 where
  one t = do
    r <- readItemShipped f pref t.url Nothing
    pure $ case r of
      Right rec -> Row t (Read rec) (maybe [] (`changesFromListing` rec) t.listed <> maybe [] (`changesBetween` rec) t.previous)
      Left (ItemUsage why) -> Row t (NeedsHtml why) []
      Left (ItemFetch why) -> Row t (Failed why) []

-- | 一覧の値と、個別ページの値の違い（個別ページの方を採る）
changesFromListing :: Candidate -> ItemRecord -> [Text]
changesFromListing c r = offerDiff "一覧" c.offer r.offer

-- | 前回の記録と、今回の記録の違い
changesBetween :: ItemRecord -> ItemRecord -> [Text]
changesBetween old new = offerDiff "前回" old.offer new.offer

offerDiff :: Text -> Offer a -> Offer b -> [Text]
offerDiff label old new =
  catMaybes
    [ case (old.priceIncl.value, new.priceIncl.value) of
        (Just a, Just b) | a /= b -> Just ("価格 " <> yen a <> "→" <> yen b <> "（" <> label <> "→個別ページ）")
        _ -> Nothing
    , case (old.stock.value, new.stock.value) of
        (Just a, Just b) | stockKind a /= stockKind b -> Just ("在庫 " <> stockShort (Just a) <> "→" <> stockShort (Just b))
        _ -> Nothing
    , case (old.shipping.value, new.shipping.value) of
        (Just a, Just b) | known a && known b && a /= b -> Just ("送料 " <> shippingShort (Just a) <> "→" <> shippingShort (Just b))
        _ -> Nothing
    ]
 where
  -- オークションの入札数や終了時刻の違いは「変化」に数えない
  stockKind :: Stock -> Int
  stockKind = \case
    InStock -> 0
    Selling -> 0
    Backorder -> 1
    OutOfStock -> 2
    Sold -> 2
    Auction _ _ -> 3
    StockUnknown -> 4
  known = \case
    ShipUnknown _ -> False
    ByDestination -> False
    BuyerPays -> False
    _ -> True

-- | verify の出力：1件1行（判定・支払額・変化・商品名・URL）
report :: [Row] -> Text
report rows =
  T.unlines $
    ("読み直した" <> show (length rows) <> "件：" <> T.intercalate "・" [k <> show n | (k, n) <- counts, n > 0])
      : "判定\t支払額\t変化\t商品名\tURL"
      : map line rows
 where
  bucket r = case r.outcome of
    Read rec -> confirmationMark rec.confirmation
    NeedsHtml _ -> "ブラウザで開く"
    Failed _ -> "失敗"
  counts = [(k, length (filter ((== k) . bucket) rows)) | k <- ["確認済み", "一部", "未確認", "ブラウザで開く", "失敗"]]
  listedName r = maybe "(名前なし)" (\c -> fromMaybe "(名前なし)" c.offer.name.value) r.target.listed
  line r = T.intercalate "\t" $ case r.outcome of
    Read rec ->
      [ confirmationMark rec.confirmation <> missingNote rec.confirmation
      , case (payable rec.offer, rec.offer.priceIncl.value) of
          (Just p, _) -> yen p
          (Nothing, Just p) -> yen p <> "+送料"
          _ -> "¥?"
      , if null r.changes then (if isJust r.target.previous || isJust r.target.listed then "変化なし" else "-") else T.intercalate "／" r.changes
      , clean 44 (fromMaybe (listedName r) rec.offer.name.value)
      , clean 200 r.target.url
      ]
    NeedsHtml why -> ["ブラウザで開く", "-", clean 60 why, clean 44 (listedName r), clean 200 r.target.url]
    Failed why -> ["失敗", "-", clean 60 why, clean 44 (listedName r), clean 200 r.target.url]
  missingNote = \case
    Partial ms -> "（" <> T.intercalate "・" ms <> "なし）"
    _ -> ""
