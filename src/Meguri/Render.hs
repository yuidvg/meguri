-- | 出力（純粋）。標準出力は短くし、決まった項目だけを出す。
module Meguri.Render
  ( yen
  , shippingShort
  , stockShort
  , candidateLine
  , candidateTable
  , soldTable
  , coverageSummary
  , itemReport
  , rankTable
  , entryLine
  , confirmationText
  , confirmationMark
  , renderMd
  , sitesMd
  , sitesTable
  , rankCandidates
  ) where

import Data.List (nubBy)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Meguri.Catalog
import Meguri.Health (Health (..), healthOf)
import Meguri.Rank (Entry (..), Frame (..), frameLabel, lowerBound)
import Meguri.Text (clean, shortStamp)
import Meguri.Types
import Protolude

yen :: Yen -> Text
yen (Yen n) = "¥" <> commas n
 where
  commas x = T.intercalate "," (reverse (map T.reverse (T.chunksOf 3 (T.reverse (show x)))))

shippingShort :: Maybe Shipping -> Text
shippingShort = \case
  Nothing -> "送料?"
  Just Free -> "送料無料"
  Just (Fixed y) -> "送料" <> yen y
  Just (FreeOver th fee) -> "送料" <> yen fee <> "（" <> yen th <> "以上無料）"
  Just ByDestination -> "送料は配送先しだい"
  Just BuyerPays -> "送料は購入者負担"
  Just (ShipUnknown _) -> "送料?"

stockShort :: Maybe Stock -> Text
stockShort = \case
  Nothing -> "在庫?"
  Just InStock -> "在庫あり"
  Just Backorder -> "取り寄せ"
  Just OutOfStock -> "在庫なし"
  Just Selling -> "販売中"
  Just Sold -> "売却済み"
  Just (Auction b e) -> "入札" <> show b <> "・" <> shortStamp e <> "終了"
  Just StockUnknown -> "在庫?"

-- | 支払額の安い順。送料が分からないものは、税込価格を「少なくともこの額」として並べ、
-- 同じ額なら支払額の分かる方を先にする（送料が配送先しだいの店を、後ろに追いやらない）。
-- 同じURLと、同じ在庫（共通の在庫番号）は1つにして、安い方を残す。
rankCandidates :: [Candidate] -> [Candidate]
rankCandidates = nubBy sameStock . sortOn key . nubBy ((==) `on` (\c -> c.offer.url))
 where
  sameStock a b = isJust a.offer.keys.sharedStockId && a.offer.keys.sharedStockId == b.offer.keys.sharedStockId
  key c = case (payable c.offer, c.offer.priceIncl.value <|> c.offer.priceExcl.value) of
    (Just p, _) -> (0 :: Int, p, 0 :: Int)
    (Nothing, Just p) -> (0, p, 1)
    (Nothing, Nothing) -> (1, 0, 0)

-- | 1候補1行：サイト・支払額・送料・在庫・切り詰めた商品名・URL
candidateLine :: Candidate -> Text
candidateLine c =
  let o = c.offer
      pay = case (payable o, o.priceIncl.value, o.priceExcl.value) of
        (Just p, _, _) -> yen p
        (Nothing, Just p, _) -> yen p <> "+送料"
        (Nothing, Nothing, Just p) -> yen p <> "(税抜)"
        _ -> "¥?"
   in T.intercalate
        "\t"
        [ o.site <> maybe "" (\s -> ":" <> clean 16 s) (if o.site == "kakaku" then o.seller.value else Nothing)
        , pay
        , shippingShort o.shipping.value
        , stockShort o.stock.value
        , clean 44 (fromMaybe "(名前なし)" o.name.value)
        , clean 200 o.url
        ]

candidateTable :: Int -> [Candidate] -> Text
candidateTable = offerTable "候補（未確認）" "　※一覧の値。個別ページで確かめるまで確定ではない"

-- | 売れた価格の表。件数・最安・中央値・最高を添える（価格だけ。送料は入れない）。
soldTable :: Int -> [Candidate] -> Text
soldTable limit cs =
  let prices = sort (mapMaybe (\c -> c.offer.priceIncl.value) (rankCandidates cs))
      n = length prices
      median
        | n == 0 = Nothing
        | odd n = Just (prices L.!! (n `div` 2))
        | otherwise = Just (Yen ((unYen (prices L.!! (n `div` 2 - 1)) + unYen (prices L.!! (n `div` 2))) `div` 2))
      stats = case (head prices, median, lastMay prices) of
        (Just lo, Just mid, Just hi) -> "　最安" <> yen lo <> "・中央値" <> yen mid <> "・最高" <> yen hi <> "（落札価格。送料は別）"
        _ -> ""
   in offerTable "売れた価格" stats limit cs

offerTable :: Text -> Text -> Int -> [Candidate] -> Text
offerTable label remark limit cs =
  let ranked = rankCandidates cs
      shown = take limit ranked
   in T.unlines $
        (label <> show (length ranked) <> "件" <> (if length ranked > limit then "のうち、安い順に" <> show limit <> "件" else "") <> remark)
          : "サイト\t支払額\t送料\t在庫\t商品名\tURL"
          : map candidateLine shown

statusShort :: Status -> Text
statusShort = \case
  Found n low -> "取得" <> show n <> "件" <> maybe "" (\y -> "・最安" <> yen y) low
  Zero -> "0件"
  Blocked why -> "ブロック：" <> clean 50 why
  NeedsBrowser why -> "ブラウザで開く：" <> clean 60 why
  Stopped day why -> "停止中（" <> day <> "）：" <> clean 50 why
  OutOfScope why -> "対象外：" <> clean 50 why
  Error why -> "失敗：" <> clean 60 why

kindShort :: Kind -> Text
kindShort = \case
  New -> "新"
  Used -> "中古"

-- | 「どこまで探したか」の集計。サイトごとに1行にまとめる（詳しい中身は coverage.jsonl）。
coverageSummary :: [Coverage] -> Text
coverageSummary cov =
  T.unlines $
    ("どこまで探したか：" <> T.intercalate "・" [k <> show n | (k, n) <- counts, n > 0] <> "（サイト×検索語×種類の行数）")
      : map siteLine grouped
 where
  bucket c = case c.status of
    Found _ _ -> "取得" :: Text
    Zero -> "0件"
    Blocked _ -> "ブロック"
    NeedsBrowser _ -> "ブラウザで開く"
    Stopped _ _ -> "停止中"
    OutOfScope _ -> "対象外"
    Error _ -> "失敗"
  counts = [(k, length (filter ((== k) . bucket) cov)) | k <- ["取得", "0件", "ブラウザで開く", "ブロック", "停止中", "対象外", "失敗"]]
  order = nubBy (==) (map (.site) cov)
  bySite = Map.fromListWith (flip (<>)) [(c.site, [c]) | c <- cov]
  grouped = [(s, Map.findWithDefault [] s bySite) | s <- order]
  siteLine (s, rows) =
    let many = length (nubBy (==) (map (.query) rows)) > 1
        -- 価格以外の情報源（リコールなど）には、新品・中古の別が無い
        kindOf c = if (categoryOf c.site) == Just NonPrice then "" else kindShort c.kind <> " "
        part c = (if many then "「" <> clean 16 c.query <> "」" else "") <> kindOf c <> statusShort c.status
        needsUrl c = case c.status of
          NeedsBrowser _ -> True
          Stopped _ _ -> True
          _ -> False
        -- 同じ状態の行（ブラウザで開く・対象外など）は、最初の1つだけ書く
        distinct = nubBy ((==) `on` (\c -> (bucket c, if isFound c then Just (c.query, c.kind) else Nothing))) rows
        isFound c = case c.status of
          Found _ _ -> True
          Zero -> True
          Error _ -> True
          _ -> False
        urls = take 2 [u | c <- rows, needsUrl c, Just u <- [c.url]]
     in T.intercalate "\t" ([s, T.intercalate " ／ " (map part distinct)] <> urls)

categoryOf :: Text -> Maybe Category
categoryOf s = (.category) <$> lookupSite s

-- | item の出力：基準の項目を、出どころつきで1行ずつ
itemReport :: ItemRecord -> Text
itemReport r =
  let o = r.offer
      row :: Text -> Field ItemSource a -> (a -> Text) -> Text
      row label f render =
        T.intercalate
          "\t"
          [ label
          , maybe "null" (clean 90 . render) f.value
          , sourceText f.source
          , clean 80 (fromMaybe "" f.note)
          ]
   in T.unlines
        [ "判定\t" <> confirmationText r.confirmation
        , "サイト\t" <> o.site <> "\t取得 " <> shortStamp o.at
        , "URL\t" <> clean 200 o.url
        , row "名前" o.name identity
        , row "税込価格" o.priceIncl yen
        , row "税抜価格" o.priceExcl yen
        , row "送料" o.shipping (shippingShort . Just)
        , "支払額\t" <> maybe "null（価格か送料が分からない）" yen (payable o)
        , row "在庫" o.stock (stockShort . Just)
        , row "販売者" o.seller identity
        , row "画像" o.image identity
        , row "購入の入口" o.buy buyText
        , row "ポイント(参考)" o.points show
        , row "状態" o.condition identity
        , "手がかり\tJAN=" <> fromMaybe "-" o.keys.jan <> " 型番=" <> clean 30 (fromMaybe "-" o.keys.model) <> " 商品番号=" <> clean 40 (fromMaybe "-" o.keys.siteItemId) <> maybe "" (" 共通在庫番号=" <>) o.keys.sharedStockId
        ]

buyText :: BuyEntry -> Text
buyText = \case
  BuyActive t -> t <> "（有効）"
  BuyDisabled t -> t <> "（無効）"

confirmationText :: Confirmation -> Text
confirmationText = \case
  Confirmed -> "confirmed（確認済み）"
  Partial ms -> "partial（足りない：" <> T.intercalate "・" ms <> "）"
  Unconfirmed -> "unconfirmed（未確認）"

sourceText :: ItemSource -> Text
sourceText = \case
  ItemPage m -> "個別ページ/" <> methodText m
  Captured m -> "保存したHTML/" <> methodText m

methodText :: Method -> Text
methodText = \case
  JsonLd -> "JSON-LD"
  Microdata -> "microdata"
  OpenGraph -> "OGP"
  NextData -> "__NEXT_DATA__"
  DataAttr -> "data属性"
  EmbeddedJson -> "埋め込みJSON"
  Body -> "本文"
  ShippingCalc -> "送料計算"

-- Markdown ------------------------------------------------------------------

cell :: Text -> Text
cell = T.replace "|" "｜" . T.replace "\n" " "

-- | rank の出力：枠ごとに、判定つきで1件1行
rankTable :: Int -> [Entry] -> Text
rankTable limit entries =
  T.unlines $
    concat
      [ (("【" <> frameLabel fr <> "】" <> show (length es) <> "件" <> (if length es > limit then "のうち" <> show limit <> "件" else "")) : map entryLine (take limit es)) <> [""]
      | fr <- [minBound .. maxBound]
      , let es = filter ((== fr) . (.frame)) entries
      , not (null es)
      ]
      <> ["判定：確認済み＝個別ページで7項目すべて読めた／一部＝個別ページを読んだが足りない項目がある／未確認＝一覧の値だけ"]

entryLine :: Entry -> Text
entryLine e =
  T.intercalate
    "\t"
    [ confirmationMark e.confirmation
    , e.site
    , case (e.payable, e.priceIncl, e.priceExcl) of
        (Just p, _, _) -> yen p
        (Nothing, Just p, _) -> yen p <> "+送料"
        (Nothing, Nothing, Just p) -> yen p <> "(税抜)"
        _ -> "¥?"
    , shippingShort e.shipping
    , stockShort e.stock
    , clean 44 (fromMaybe "(名前なし)" e.name) <> (if null e.duplicates then "" else "（同じ在庫がほかに" <> show (length e.duplicates) <> "か所）")
    , clean 200 e.url
    ]

confirmationMark :: Confirmation -> Text
confirmationMark = \case
  Confirmed -> "確認済み"
  Partial _ -> "一部"
  Unconfirmed -> "未確認"

-- | スキルの出力の表（候補・未確認の候補・どこまで探したか）。公式と注記の列は空けておく。
--
-- 候補の表に入るのは、個別ページで確認済みになったものだけ。枠ごとに、支払額の安い順。
renderMd :: Int -> [Entry] -> [Coverage] -> Text
renderMd limit entries cov =
  T.unlines $
    [ "## 候補"
    , ""
    , "個別ページで、名前・税込価格・送料・在庫・販売者・画像・購入の入口をすべて読めたもの。枠ごとに、支払額の安い順。"
    ]
      <> (if null confirmed then ["", "（まだ無い。`meguri verify` か `meguri item <URL>` で個別ページを読む）"] else [])
      <> concat
        [ [ ""
          , "### " <> frameLabel fr
          , ""
          , "| 画像 | 商品・型番・色 | 支払額（税込・送料込み） | ポイント（参考） | 在庫・確認時刻 | 公式 | 購入 | 注記 |"
          , "|---|---|---|---|---|---|---|---|"
          ]
            <> map confirmedRow es
        | fr <- [minBound .. maxBound]
        , let es = filter ((== fr) . (.frame)) confirmed
        , not (null es)
        ]
      <> [ ""
         , "## 未確認の候補"
         , ""
         , "個別ページを読めていないもの、読めても項目が足りないもの。値の出どころを書く。"
         , ""
         , "| 画像 | 商品 | 価格・送料 | 在庫 | 値の出どころ | 足りない項目 | ページ |"
         , "|---|---|---|---|---|---|---|"
         ]
      <> map unconfirmedRow (partials <> take limit unread)
      <> [ ""
         , "## どこまで探したか"
         , ""
         , "| 区分 | サイト | 検索語 | 結果（件数・最安／0件／見られない） | 見た方法・代わりの道 | 確認時刻 |"
         , "|---|---|---|---|---|---|"
         ]
      <> map covRow (filter (not . superseded) cov)
 where
  -- 別のコマンド（recall など）で調べがついたサイトの「対象外」の行は出さない
  superseded c = case c.status of
    OutOfScope _ -> any (\d -> d.site == c.site && fetched d.status) cov
    _ -> False
  fetched = \case
    Found _ _ -> True
    Zero -> True
    _ -> False
  confirmed = [e | e <- entries, e.confirmation == Confirmed]
  partials = [e | e <- entries, e.confirmation /= Confirmed, e.fromPage]
  unread = sortOn (\e -> (isNothing (lowerBound e), lowerBound e)) [e | e <- entries, not e.fromPage, e.frame /= Gone]
  img :: Maybe Text -> Text
  img = maybe "なし" (\u -> "![](" <> u <> ")")
  confirmedRow e =
    row
      [ img e.image
      , cell (clean 80 (fromMaybe "" e.name)) <> maybe "" (\c -> "（" <> cell (clean 20 c) <> "）") e.condition
      , maybe "未確認" yen e.payable <> "（" <> maybe "?" yen e.priceIncl <> "＋" <> shippingShort e.shipping <> "）"
      , maybe "未確認" (\p -> show p <> "pt") e.points
      , stockShort e.stock <> " " <> shortStamp e.at
      , ""
      , "[" <> cell (clean 20 (fromMaybe e.site e.seller)) <> "](" <> e.url <> ")"
      , if null e.duplicates then "" else "同じ在庫がほかに" <> show (length e.duplicates) <> "か所"
      ]
  unconfirmedRow e =
    row
      [ img e.image
      , cell (clean 80 (fromMaybe "(名前なし)" e.name))
      , maybe (maybe "未確認" (\p -> yen p <> "（税抜）") e.priceExcl) yen e.priceIncl <> "・" <> shippingShort e.shipping
      , stockShort e.stock
      , (if e.fromPage then "個別ページ（" else "検索結果の一覧（") <> e.site <> "） " <> shortStamp e.at
      , case e.confirmation of
          Partial ms -> cell (T.intercalate "・" ms)
          Unconfirmed -> if e.fromPage then "すべて" else "個別ページを未読"
          Confirmed -> ""
      , "[開く](" <> e.url <> ")"
      ]
  covRow c =
    let def = lookupSite c.site
     in row
          [ maybe "" (categoryName . (.category)) def
          , maybe c.site (.label) def
          , cell (clean 30 c.query) <> "（" <> kindShort c.kind <> "）"
          , cell (statusShort c.status)
          , cell c.method <> maybe "" (\u -> " [URL](" <> u <> ")") (if needsLink c.status then c.url else Nothing)
          , shortStamp c.at
          ]
  needsLink = \case
    NeedsBrowser _ -> True
    Stopped _ _ -> True
    Blocked _ -> True
    _ -> False
  row cols = "| " <> T.intercalate " | " cols <> " |"

-- | カタログと健康診断の結果から channels.md を作る
sitesMd :: [Health] -> Text
sitesMd health =
  T.unlines $
    [ "# 販路の一覧"
    , ""
    , "このファイルは `meguri sites --format md` の出力。直すときは `src/Meguri/Catalog.hs` を直して、作り直す。"
    , ""
    , "- 注記は、確かめた日に実際に開いて分かったことをもとにしたヒントで、決まった手順ではない。"
    , "- 見出しの（ ）は、個別ページまで読めた方法。"
    , "  - HTTP可：HTMLを取るだけで読めた。"
    , "  - 個別ページだけHTTP可：検索結果はJSで描画されるが、個別ページはHTMLを取るだけで読めた。"
    , "  - ブラウザ要：JSで描画されるので、ヘッドレスでも構わないのでブラウザが要る。"
    , "  - 通常のブラウザ要：HTTP もヘッドレスも拒否されたが、画面ありの通常のブラウザでは読めた。"
    , "  - 停止中：その日は止まっていた。　不可：どの方法でも読めなかった。　対象外：買う販路ではない。"
    , "- `{q}` は検索語。サイトの文字コード（UTF-8 でなければ見出しに書く）で % エンコードする。`meguri search` が組み立てる。"
    , "- ボット確認やCAPTCHAが出たら、解かず、回避もしない。"
    , "- 価格比較サイトやモール支店で見た値は、その店のページを見たことにならない（未確認として扱う）。"
    , "- 「診断」は、`meguri check` の直近の結果（meguri のUAで、検索1回・個別1件を当てたもの）。「要確認」は、カタログの段と合わなかったサイト。"
    , ""
    , "## 見えないときの見分け方"
    , ""
    , "| 見え方 | よくある原因 | 試すこと |"
    , "|---|---|---|"
    , "| HTTP・ヘッドレスで 403、Access Denied、HTTP/2 エラー、空のページ | 自動アクセスへの制限（Akamai・Cloudflare など） | 通常のブラウザで開く |"
    , "| 通常のブラウザでも Access Denied、ボット確認で止まる | 強い制限 | そのサイトは「見られない」と書く。モール支店や価格.comで候補だけ拾い、値は未確認として扱う |"
    , "| 「メンテナンス」「停止中」のページ | サイト側の停止 | 日を改めて開き直す。モール支店があればそちらを見る |"
    , "| 検索結果が文字化けする、または0件や無関係な結果になる | 検索語の文字コードの違い | Shift_JIS でエンコードし直す |"
    , "| 件数が不自然に多い | パラメータの名前の間違い | 名前を直す（例：トレジャーファクトリーの word=） |"
    , "| 「該当なし」の表示、0件ページ（サイトによっては404） | 本当の0件 | 別の型番・日本語名で探し、それでも無ければ0件と書く |"
    ]
      <> concatMap section [minBound .. maxBound]
      <> ["", "## 一覧から外したもの", "", excludedNote]
 where
  section cat =
    let ss = filter ((== cat) . (.category)) catalog
     in if null ss then [] else ["", "## " <> categoryName cat] <> concatMap entry ss
  entry s =
    [ ""
    , "### "
        <> s.label
        <> "（"
        <> tierLabel s.tier
        <> encNote s.enc
        <> "）"
    , ""
    , "- id：`" <> s.sid <> "`　扱う種類：" <> T.intercalate "・" (map kindLabel s.kinds) <> "　確かめた日：" <> show s.checked <> (if s.adapter /= s.sid then "　アダプタ：`" <> s.adapter <> "`" else "")
    ]
      <> [ "- 診断（" <> h.day <> "）：" <> h.search <> "／" <> h.item <> (if h.asExpected then "" else "　**要確認**")
         | Just h <- [healthOf health s.sid]
         ]
      <> ["- 検索：`" <> u <> "`" | Just u <- [s.searchNew]]
      <> ["- 検索（中古）：`" <> u <> "`" | Just u <- [s.searchUsed]]
      <> ["- 入り方：" <> n | Just n <- [s.searchNote]]
      <> ["- 停止の理由：" <> why | TierStopped why <- [s.tier]]
      <> ["- 個別ページのURLの形：" <> T.intercalate "、" (map (\p -> "`" <> p <> "`") s.itemPatterns) | not (null s.itemPatterns)]
      <> map ("- ＊" <>) s.hints
  encNote = \case
    Utf8 -> ""
    ShiftJis -> "・検索語は Shift_JIS"
    Cp932 -> "・検索語は cp932"
    EucJp -> "・検索語は EUC-JP"

-- | sites の表（1サイト1行）
sitesTable :: Text
sitesTable =
  T.unlines $
    "id\t名前\t区分\t段\t扱う種類\t確かめた日"
      : [ T.intercalate "\t" [s.sid, s.label, categoryName s.category, tierLabel s.tier, T.intercalate "・" (map kindLabel s.kinds), show s.checked]
        | s <- catalog
        ]
