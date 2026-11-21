{-# LANGUAGE DeriveAnyClass #-}

-- | 並べる：重複を除き、枠に分けて並べる（純粋）。
--
-- 確認済みかどうかは、個別ページの記録（'Reading'）が持つ判定をそのまま使う。
-- 一覧の候補（'Sighting'）は、ここでも未確認のまま。
module Meguri.Arrange
  ( Frame (..)
  , frameLabel
  , Finding (..)
  , fromReading
  , fromSighting
  , arrange
  , latestReadings
  , lowerBound
  , classify
  ) where

import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Meguri.Catalog (Channel (..), SiteKind (..), lookupSite)
import Meguri.Text (toHalfWidth)
import Meguri.Types
import Protolude

-- | 候補を置く枠
data Frame
  = -- | すぐ買える確定値（新品・即決）
    BuyNow
  | -- | 入札次第（オークション）
    Bidding
  | -- | 訳あり（ジャンク・アウトレット・展示品など）
    Flawed
  | -- | 中古
    Secondhand
  | -- | いまは買えない（売り切れ・終了）
    Gone
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

frameLabel :: Frame -> Text
frameLabel = \case
  BuyNow -> "すぐ買える（新品・即決）"
  Bidding -> "入札次第（オークション。いまの価格で落札できた場合の額）"
  Flawed -> "訳あり（ジャンク・アウトレット・展示品など）"
  Secondhand -> "中古"
  Gone -> "いまは買えない（売り切れ・終了）"

-- | 並べるための1件。個別ページの記録からも、一覧の候補からも作る。
data Finding = Finding
  { frame :: Frame
  , confirmation :: Confirmation
  , site :: Text
  , url :: Text
  , name :: Maybe Text
  , payable :: Maybe Yen
  , priceIncl :: Maybe Yen
  , priceExcl :: Maybe Yen
  , shipping :: Maybe Shipping
  , stock :: Maybe Stock
  , seller :: Maybe Text
  , image :: Maybe Text
  , points :: Maybe Int
  , condition :: Maybe Text
  , -- | 購入の入口（カートや購入のボタン）。押さない
    buy :: Maybe BuyEntry
  , keys :: Keys
  , -- | 値を読んだ時刻（JST）。個別ページなら、そのページを確かめた時刻
    at :: Text
  , -- | 個別ページの記録から作ったか（一覧の候補なら False）
    fromPage :: Bool
  , -- | 同じ在庫が出ていた別の窓口（URL）
    duplicates :: [Text]
  , -- | 読めなかった項目の理由、値についての但し書き、一覧の値からの変化
    notes :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

fromReading :: Reading -> Finding
fromReading r = (entry r.confirmation True False r.offer) {notes = fieldNotes r.offer}

-- | 読めなかった項目の理由と、値についての但し書き（「サイトの決まりとして税込と読んだ」など）
fieldNotes :: Offer s -> [Text]
fieldNotes o =
  catMaybes
    [ note "名前" o.name
    , note "税込価格" o.priceIncl
    , case o.shipping.value of
        Just (ShipUnknown why) -> Just ("送料：" <> why)
        _ -> note "送料" o.shipping
    , note "在庫" o.stock
    , note "販売者" o.seller
    , note "画像" o.image
    , note "購入の入口" o.buy
    ]
 where
  note :: Text -> Field s a -> Maybe Text
  note l f = case f.note of
    Just n | n `notElem` boring -> Just (l <> "：" <> n)
    _ -> Nothing
  -- どこから読んだかだけの注記は、出力に出さない（readings.jsonl には残る）
  boring = ["読めなかった", "本文の税込の表示と同じ額", "本文の大きい表示（税込）", "直販（販売者は店そのもの）"]

-- | 一覧の候補は、いつも未確認
fromSighting :: Sighting -> Finding
fromSighting c = entry Unconfirmed False (c.market == Used) c.offer

entry :: Confirmation -> Bool -> Bool -> Offer s -> Finding
entry confirmation fromPage usedSearch o =
  Finding
    { frame = classify usedSearch o
    , confirmation
    , site = o.site
    , url = o.url
    , name = o.name.value
    , payable = payable o
    , priceIncl = o.priceIncl.value
    , priceExcl = o.priceExcl.value
    , shipping = o.shipping.value
    , stock = o.stock.value
    , seller = o.seller.value
    , image = o.image.value
    , points = o.points.value
    , condition = o.condition.value
    , buy = o.buy.value
    , keys = o.keys
    , at = o.at
    , fromPage
    , duplicates = []
    , notes = []
    }

-- | 枠を決める（決まった規則）。
--
-- 1. 売り切れ・終了 → いまは買えない
-- 2. オークション → 入札次第
-- 3. 名前か状態に「ジャンク」「訳あり」などがある → 訳あり
-- 4. 状態が中古、名前に「中古」、中古の検索で見つけた、中古だけを扱うサイト → 中古
--    （ただし、状態が新品・未使用と読めたものは除く）
-- 5. それ以外 → すぐ買える
classify :: Bool -> Offer s -> Frame
classify usedSearch o
  | o.stock.value `elem` [Just Sold, Just OutOfStock] = Gone
  | isAuction = Bidding
  | has flawedWords = Flawed
  | saysNew = BuyNow
  | has usedWords || usedSearch || usedOnlySite = Secondhand
  | otherwise = BuyNow
 where
  isAuction = case o.stock.value of
    Just (Auction _ _) -> True
    _ -> False
  text = T.toLower (toHalfWidth (fromMaybe "" o.name.value <> " " <> fromMaybe "" o.condition.value))
  has = any (`T.isInfixOf` text)
  flawedWords = ["ジャンク", "訳あり", "わけあり", "難あり", "アウトレット", "展示品", "動作未確認", "部品取り", "現状品", "b級", "damaged"]
  usedWords = ["中古", "used", "ランク", "rank", "傷や汚れ", "やや傷", "目立った傷", "全体的に状態が悪い", "good", "acceptable", "refurbished", "整備済"]
  saysNew = maybe False (\c -> any (`T.isInfixOf` T.toLower c) ["新品", "未使用", "new"]) o.condition.value && not (has ["中古"])
  usedOnlySite = case lookupSite o.site of
    Just def -> KNew `notElem` def.kinds && any (`elem` def.kinds) [KUsed, KAuction, KP2P]
    Nothing -> False

-- | 同じページを読み直した記録は、新しい方を残す（入力は古い順）
latestReadings :: [Reading] -> [Reading]
latestReadings = Map.elems . Map.fromList . map (\r -> (r.offer.url, r))

-- | 少なくとも払う額：支払額が分かればそれ、分からなければ価格
lowerBound :: Finding -> Maybe Yen
lowerBound e = e.payable <|> e.priceIncl <|> e.priceExcl

-- | 個別ページの記録と一覧の候補をまとめ、重複を除いて、枠ごとに安い順に並べる。
--
-- * 同じURLは、個別ページの記録を残す。
-- * 同じ在庫（共通の在庫番号）は1つにして、確認済みの方、次に安い方を残す。
-- * 枠の中は、確認済み（支払額の安い順）→ 一部だけ読めたもの → 未確認、の順。
arrange :: [Reading] -> [Sighting] -> [Finding]
arrange items cands =
  let fromItems = map fromReading (latestReadings items)
      seen = map (.url) fromItems
      fromLists = nubBy ((==) `on` (.url)) [e | e <- map fromSighting cands, e.url `notElem` seen]
      ordered = sortOn sortKey (fromItems <> fromLists)
   in sortOn (\e -> (e.frame, sortKey e)) (mergeSameStock ordered)
 where
  sortKey e = (confirmationRank e.confirmation, isNothing (lowerBound e), lowerBound e, isNothing e.payable)
  confirmationRank = \case
    Confirmed -> 0 :: Int
    Partial _ -> 1
    Unconfirmed -> 2
  -- 並べた順（良い順）に見て、同じ在庫の2つ目からは、1つ目の duplicates に入れる
  mergeSameStock = foldl' merge []
  merge acc e = case e.keys.sharedStockId of
    Just k | any (\x -> x.keys.sharedStockId == Just k) acc -> map (\x -> if x.keys.sharedStockId == Just k then x {duplicates = x.duplicates <> [e.url]} else x) acc
    _ -> acc <> [e]
