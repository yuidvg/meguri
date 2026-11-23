-- | search の出力（純粋）。確かめた候補を、読んだ項目と確かめた時刻つきで、1件1行で出す。
module Meguri.Report
  ( table
  , resultLine
  , jsonLines
  , markdown
  ) where

import Data.Aeson (ToJSON, Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Meguri.Arrange (Finding (..), frameLabel)
import Meguri.Catalog (catalog)
import Meguri.Search (Report (..))
import Meguri.Selection (Excluded (..), Selected (..))
import Meguri.Render
import Meguri.Run (line)
import Meguri.Text (clean, shortStamp)
import Meguri.Types
import Meguri.Read (Checked (..), Page (..), Unread (..))
import Protolude

-- | 個別ページを読んだもの
readFindings :: Report -> [Finding]
readFindings r = [e | e <- r.findings, e.fromPage]

-- | 読めなかった理由。取りに行かなかったなら、その理由。取りに行ったなら、うまくいかなかった最初の取得
data Why = Why
  { -- | 種類：needs_browser・blocked_before・blocked・stopped・timed_out・failed など
    kind :: Text
  , -- | 説明（分け方の名前は付けない）
    reason :: Text
  , -- | 「ブロック：…」の形
    short :: Text
  }

unreadWhy :: Did -> Why
unreadWhy = \case
  Skipped why -> Why (skipKind why) (skipText why) (skipShort why)
  Tried as -> case head (troubles as) of
    Just a -> Why (failureKind a) (troubleText a) (troubleShort a)
    Nothing -> Why "failed" "読めなかった" "失敗：読めなかった"

-- | 表（既定の出力）。reads は、販路と新品・中古の組ごとに読んだ個別ページの数。limit は、枠ごとに出す行数
table :: Int -> Int -> Report -> Text
table reads limit r
  | null r.checked && null r.unread = T.intercalate "\n" (filter (not . T.null) [T.stripEnd (sightingTable (limit * 3) r.accepted.chosen), listOnly, scopeLine r, acceptLine r]) <> "\n\n" <> coverageSummary r.coverage
  | otherwise = T.intercalate "\n" (filter (not . T.null) [readPart, unreadPart, restLine, scopeLine r, acceptLine r]) <> "\n\n" <> coverageSummary r.coverage
 where
  listOnly = "（個別ページは読んでいない。--read N で、販路と新品・中古の組ごとに安い順で N 件を読む）"
  es = readFindings r
  nOf p = length (filter p es)
  readPart =
    T.intercalate "\n" $
      [ "確かめた候補"
          <> show (length es)
          <> "件：確認済み"
          <> show (nOf ((== Confirmed) . (.confirmation)))
          <> "・一部"
          <> show (nOf ((/= Confirmed) . (.confirmation)))
          <> "（個別ページを読んだもの。「確認」の列が、そのページを読んだ時刻）"
      ]
        <> (if null es then [] else ["判定\tサイト\t支払額\t税込価格\t送料\t在庫\t状態\t販売者\t確認\t商品名\tURL\t注記"])
        <> concat
          [ ("【" <> frameLabel fr <> "】" <> show (length inFrame) <> "件" <> (if length inFrame > limit then "のうち" <> show limit <> "件" else ""))
              : map resultLine (take limit inFrame)
          | fr <- [minBound .. maxBound]
          , let inFrame = filter ((== fr) . (.frame)) es
          , not (null inFrame)
          ]
  unread = r.unread
  unreadPart
    | null unread = ""
    | otherwise =
        T.intercalate "\n" $
          ("個別ページを読めなかった候補" <> show (length unread) <> "件（一覧の値のまま）")
            : "理由\tサイト\t一覧の価格\t商品名\tURL"
            : [ T.intercalate
                  "\t"
                  [ (unreadWhy u.did).short
                  , maybe "-" (\c -> c.offer.site) u.page.sighting
                  , maybe "-" yen (u.page.sighting >>= \c -> c.offer.priceIncl.value)
                  , clean 44 (fromMaybe "(名前なし)" (u.page.sighting >>= \c -> c.offer.name.value))
                  , clean 200 u.page.url
                  ]
              | u <- take (limit * 2) unread
              ]
  rest = length [e | e <- r.findings, not e.fromPage]
  restLine
    | rest == 0 = ""
    | otherwise =
        "確かめていない候補"
          <> show rest
          <> "件（一覧の値だけ。探している物と認めたもののうち、販路と新品・中古の組ごとに安い順で"
          <> show reads
          <> "件ずつ読んだ。--read N で増やす）"

-- | どこを：カタログの販路のうち、いくつに当たったか。どの条件・判定が、いくつ外したか
scopeLine :: Report -> Text
scopeLine r
  | null r.channels.excluded =
      "どこを：カタログの全" <> show (length catalog) <> "販路（--only・--skip・--category・--judge-channel で選ぶ）"
  | otherwise =
      "どこを：カタログの"
        <> show (length catalog)
        <> "販路 → "
        <> T.intercalate " → " [d.by <> " で" <> show d.count <> "販路を外す" | d <- r.channels.excluded]
        <> " → "
        <> show (length r.channels.chosen)
        <> "販路"

-- | 何を：一覧で見かけたもののうち、いくつを探している物と認めたか。どの条件・判定が、いくつ外したか
acceptLine :: Report -> Text
acceptLine r
  | null r.accepted.excluded = "何を：選び方なし（安い順の上に付属品が並ぶなら、--min-price・--has・--not・--judge で選ぶ）"
  | otherwise =
      "何を：一覧で見かけた"
        <> show (length r.sightings)
        <> "件 → "
        <> T.intercalate " → " [d.by <> " で" <> show d.count <> "件を外す" | d <- r.accepted.excluded]
        <> " → "
        <> show (length r.accepted.chosen)
        <> "件"
        <> ( case [d | d <- r.rechecked, d.count > 0] of
               [] -> ""
               ds -> "。個別ページの値では、" <> T.intercalate "・" [d.by <> " で" <> show d.count <> "件" | d <- ds] <> "を外した"
           )

-- | 確かめた候補の1行：判定・サイト・支払額・税込価格・送料・在庫・状態・販売者・確かめた時刻・商品名・URL・注記
resultLine :: Finding -> Text
resultLine e =
  T.intercalate
    "\t"
    [ confirmationMark e.confirmation <> missing
    , e.site
    , case (e.payable, e.priceIncl, e.priceExcl) of
        (Just p, _, _) -> yen p
        (Nothing, Just p, _) -> yen p <> "+送料"
        (Nothing, Nothing, Just p) -> yen p <> "(税抜)+送料"
        _ -> "¥?"
    , maybe (maybe "-" (\p -> yen p <> "(税抜)") e.priceExcl) yen e.priceIncl
    , shippingShort e.shipping
    , stockShort e.stock
    , maybe "-" (clean 24) e.condition
    , maybe "-" (clean 20) e.seller
    , shortStamp e.at
    , clean 44 (fromMaybe "(名前なし)" e.name)
    , clean 200 e.url
    , clean 130 (T.intercalate "／" (["同じ在庫がほかに" <> show (length e.duplicates) <> "か所" | not (null e.duplicates)] <> e.notes))
    ]
 where
  missing = case e.confirmation of
    Partial ms -> "（" <> T.intercalate "・" ms <> "なし）"
    _ -> ""

-- | JSON Lines：確かめた候補（meguri.result/2）、読めなかった候補（meguri.unread/2）、
-- 集計（meguri.summary/1）、どこまで探したか（meguri.coverage/3）の順。
--
-- 確かめた候補と読めなかった候補には、取得の並び（attempts）が付く。読めなかった候補には、
-- 取りに行かなかった理由（skipped）か、うまくいかなかった取得が入る。ブロックの事実（block）は、その取得の中にある
jsonLines :: Report -> [ByteString]
jsonLines r =
  [line "meguri.result/2" (withPairs e ["attempts" .= Map.findWithDefault [] e.url attemptsByUrl]) | e <- es]
    <> [ line "meguri.unread/2" $
           withPairs
             u.did
             [ "url" .= u.page.url
             , "site" .= fmap (\c -> c.offer.site) u.page.sighting
             , "name" .= (u.page.sighting >>= \c -> c.offer.name.value)
             , "listedPrice" .= (u.page.sighting >>= \c -> c.offer.priceIncl.value)
             , "why" .= why.kind
             , "reason" .= why.reason
             ]
       | u <- r.unread
       , let why = unreadWhy u.did
       ]
    <> [line "meguri.summary/1" summary]
    <> map (line "meguri.coverage/3") r.coverage
 where
  es = readFindings r
  attemptsByUrl = Map.fromList [(c.reading.offer.url, c.attempts) | c <- r.checked]
  -- JSON の対象に、項目を足す
  withPairs :: ToJSON a => a -> [(KM.Key, Value)] -> Value
  withPairs a pairs = case toJSON a of
    Object o -> Object (KM.union (KM.fromList pairs) o)
    v -> v
  summary :: Value
  summary =
    object
      [ "channels" .= object ["catalog" .= length catalog, "searched" .= length r.channels.chosen, "excluded" .= r.channels.excluded]
      , "sighted" .= length r.sightings
      , "accepted" .= length r.accepted.chosen
      , "excluded" .= r.accepted.excluded
      , "excludedAfterRead" .= r.rechecked
      , "read" .= length es
      , "confirmed" .= length (filter ((== Confirmed) . (.confirmation)) es)
      , "partial" .= length (filter ((/= Confirmed) . (.confirmation)) es)
      , "unread" .= length r.unread
      , "unconfirmed" .= length [e | e <- r.findings, not e.fromPage]
      ]

-- | Markdown：候補（確認済み）・未確認の候補・どこまで探したか、の3つの表
markdown :: Int -> Report -> Text
markdown limit r = renderMd limit r.findings (Map.fromList [(u.page.url, (unreadWhy u.did).short) | u <- r.unread]) r.coverage
