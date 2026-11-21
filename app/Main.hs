-- | meguri：購入調査の決まった作業を受け持つ CLI
module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Meguri.Catalog
import Meguri.Check qualified as Check
import Meguri.Fetch.Http
import Meguri.Health (Health (..), merge, moduleSource, parseModule, recorded)
import Meguri.Item
import Meguri.Extract.Html (parse)
import Meguri.Fetch.Encoding (decodeBody)
import Meguri.Rank (rank)
import Meguri.Recall qualified as Recall
import Meguri.Render
import Meguri.Run
import Meguri.Search
import Meguri.Ship (Pref, parsePref, shipPlan, supported)
import Meguri.Text (jstStamp)
import Meguri.Types
import Meguri.Verify qualified as Verify
import Options.Applicative
import Protolude
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hSetEncoding, utf8)

data Format = FTable | FJsonl | FMd
  deriving stock (Eq, Show)

data Common = Common
  { format :: Format
  , fresh :: Bool
  , run :: Maybe Text
  }

data Command
  = CmdSites Common
  | CmdSearch Common SearchOpts Int (Maybe Int) (Maybe Int)
  | CmdSold Common [Text] Int (Maybe Int) (Maybe Int)
  | CmdItem Common Text (Maybe FilePath)
  | CmdRender Common Int (Maybe Int) (Maybe Int)
  | CmdRank Common Int (Maybe Int) (Maybe Int)
  | CmdVerify Common Int (Maybe Int) (Maybe Int)
  | CmdShip Common Text
  | CmdCheck Common Bool Bool [Text]
  | CmdRecall Common [Text] Int

common :: Parser Common
common =
  Common
    <$> option
      (eitherReader readFormat)
      (long "format" <> metavar "table|jsonl|md" <> value FTable <> help "出力の形（既定は table）")
    <*> switch (long "fresh" <> help "キャッシュを使わない")
    <*> optional (strOption (long "run" <> metavar "ID" <> help "実行の id（既定は直前の実行）"))
 where
  readFormat = \case
    "table" -> Right FTable
    "jsonl" -> Right FJsonl
    "md" -> Right FMd
    other -> Left ("format は table・jsonl・md のどれか：" <> other)

commaList :: Mod OptionFields Text -> Parser [Text]
commaList m = concatMap (filter (not . T.null) . T.splitOn ",") <$> many (strOption m)

searchP :: Parser Command
searchP =
  (\c qs k only skip cat limit lo hi -> CmdSearch c (SearchOpts qs k only skip cat) limit lo hi)
    <$> common
    <*> some (strArgument (metavar "語..." <> help "検索語（型番・JAN・商品名など）。複数あれば結果をまとめる"))
    <*> option
      (eitherReader readKind)
      (long "kind" <> metavar "new|used|all" <> value SelAll <> help "新品・中古・両方（既定は all）")
    <*> commaList (long "only" <> metavar "SITE" <> help "このサイトだけ当たる（id。カンマ区切りか繰り返し）")
    <*> commaList (long "skip" <> metavar "SITE" <> help "このサイトを飛ばす")
    <*> optional (strOption (long "category" <> metavar "区分" <> help "区分の名前の一部（例：リユース）"))
    <*> option auto (long "limit" <> metavar "N" <> value 40 <> help "標準出力に出す候補の数（既定は 40）")
    <*> optional (option auto (long "min-price" <> metavar "円" <> help "この額より安い候補を標準出力から外す（付属品よけ）"))
    <*> optional (option auto (long "max-price" <> metavar "円" <> help "この額より高い候補を標準出力から外す"))
 where
  readKind = \case
    "new" -> Right SelNew
    "used" -> Right SelUsed
    "all" -> Right SelAll
    other -> Left ("kind は new・used・all のどれか：" <> other)

commands :: Parser Command
commands =
  hsubparser $
    mconcat
      [ command "sites" (info (CmdSites <$> common) (progDesc "カタログ（販路の一覧）を出す。--format md で channels.md を作る"))
      , command "search" (info searchP (progDesc "全サイトを並列に当たり、候補（未確認）と「どこまで探したか」を出す"))
      , command "item" (info itemP (progDesc "個別ページを読み、基準の項目を出どころつきで出す"))
      , command "render" (info renderP (progDesc "実行の結果から、出力用の表を作る（--format md）"))
      , command "verify" (info verifyP (progDesc "候補の個別ページを取り直して確認済みにし、価格や在庫の変化を出す"))
      , command "rank" (info rankP (progDesc "重複を除き、枠（すぐ買える／入札次第／訳あり／中古）に分けて並べる"))
      , command "ship" (info shipP (progDesc "送料を計算できるサイト（ハードオフ・トレジャーファクトリー）で、MEGURI_PREF の配送先までの送料を出す"))
      , command "recall" (info recallP (progDesc "消費者庁と CPSC のリコール情報を出す"))
      , command "check" (info checkP (progDesc "サイトごとに健康診断をする（検索1回・個別1件）。--write で、結果と日付をカタログと docs/channels.md に書く"))
      , command "sold" (info soldP (progDesc "売れた価格を出す（ヤフオクの落札相場）"))
      ]
 where
  priceRange what =
    (,)
      <$> optional (option auto (long "min-price" <> metavar "円" <> help ("この額より安い" <> what <> "を除く（付属品よけ）")))
      <*> optional (option auto (long "max-price" <> metavar "円" <> help ("この額より高い" <> what <> "を除く")))
  checkP =
    CmdCheck
      <$> common
      <*> switch (long "write" <> help "結果を src/Meguri/Catalog/Health.hs と docs/channels.md に書く（リポジトリの中で走らせる）")
      <*> switch (long "all" <> help "ブラウザの段のサイトにも、検索URLを1回だけ当てる（HTTPで読めるようになっていないかを見る）")
      <*> commaList (long "only" <> metavar "SITE" <> help "このサイトだけ診断する")
  shipP = CmdShip <$> common <*> strArgument (metavar "URL" <> help "個別ページのURL")
  recallP =
    CmdRecall
      <$> common
      <*> some (strArgument (metavar "語..." <> help "製品名・メーカー名など（CPSC は英語で探す）"))
      <*> option auto (long "limit" <> metavar "N" <> value 20 <> help "標準出力に出す件数（既定は 20）")
  verifyP =
    (\c n (lo, hi) -> CmdVerify c n lo hi)
      <$> common
      <*> option auto (long "top" <> metavar "N" <> value 10 <> help "候補の上位（安い順）何件の個別ページを読むか（既定は 10）。前に読んだページは、いつも読み直す")
      <*> priceRange "候補"
  rankP =
    (\c n (lo, hi) -> CmdRank c n lo hi)
      <$> common
      <*> option auto (long "limit" <> metavar "N" <> value 10 <> help "枠ごとに標準出力に出す数（既定は 10）")
      <*> priceRange "一覧の候補"
  renderP =
    CmdRender
      <$> common
      <*> option auto (long "limit" <> metavar "N" <> value 40 <> help "未確認の候補の表に載せる数（既定は 40）")
      <*> optional (option auto (long "min-price" <> metavar "円" <> help "この額より安い未確認の候補を載せない（付属品よけ）"))
      <*> optional (option auto (long "max-price" <> metavar "円" <> help "この額より高い未確認の候補を載せない"))
  itemP =
    (\c u h -> CmdItem c u h)
      <$> common
      <*> strArgument (metavar "URL" <> help "個別ページのURL")
      <*> optional (strOption (long "html" <> metavar "FILE" <> help "通常のブラウザで保存したHTML（取りに行かず、これを読む）"))
  soldP =
    (\c qs n lo hi -> CmdSold c qs n lo hi)
      <$> common
      <*> some (strArgument (metavar "語..."))
      <*> option auto (long "limit" <> metavar "N" <> value 40 <> help "標準出力に出す件数（既定は 40）")
      <*> optional (option auto (long "min-price" <> metavar "円" <> help "この額より安いものを除く（付属品よけ。集計にも効く）"))
      <*> optional (option auto (long "max-price" <> metavar "円" <> help "この額より高いものを除く"))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  let parser = info (commands <**> helper) (fullDesc <> progDesc "購入調査の決まった作業（検索URL・取得・抽出・正規化・表）を受け持つ。買わない・ログインしない。")
  case execParserPure (prefs showHelpOnEmpty) parser args of
    Success cmd -> dispatch cmd
    Failure failure -> do
      let (msg, code) = renderFailure failure "meguri"
      -- 使い方の誤りは 2。--help は 0。
      if code == ExitSuccess
        then putStrLn msg >> exitSuccess
        else hPutStrLn stderr msg >> exitWith (ExitFailure 2)
    CompletionInvoked _ -> exitSuccess

dispatch :: Command -> IO ()
dispatch = \case
  CmdSites c -> TIO.putStr $ case c.format of
    FMd -> sitesMd recorded
    _ -> sitesTable
  CmdSearch c opts limit lo hi -> do
    paths <- getPaths
    purgeOld paths
    (rid, dir) <- newRun paths
    f <- newFetcher paths.httpCache c.fresh (Just (dir </> "pages"))
    (cov, cands) <- runSearch f opts
    save dir cov cands
    output c rid dir cov (filter (inRange lo hi) cands) limit
    exitByCoverage cov
  CmdSold c qs limit lo hi -> do
    paths <- getPaths
    purgeOld paths
    -- 売れた価格は、いまの実行に足す（候補とは別のファイル）
    existing <- resolveRun paths c.run
    (rid, dir) <- maybe (newRun paths) pure existing
    f <- newFetcher paths.httpCache c.fresh (Just (dir </> "pages"))
    let jobs =
          [ Job def q Used (Just (searchUrl def.enc "https://auctions.yahoo.co.jp/closedsearch/closedsearch?p={q}" q)) Nothing
          | Just def <- [lookupSite "yahuoku"]
          , q <- qs
          ]
    (cov, sold) <- runJobs f jobs
    appendLines (dir </> "sold.jsonl") (map (line "meguri.sold/1") sold)
    let shown = filter (inRange lo hi) sold
    case c.format of
      FJsonl -> traverse_ (BC.putStrLn . line "meguri.sold/1") (rankCandidates shown)
      _ -> TIO.putStr (soldTable limit shown <> "\n" <> coverageSummary cov <> "\n実行 " <> rid <> "：" <> toS dir <> "（sold.jsonl）\n")
    exitByCoverage cov
  CmdItem c url saved -> do
    paths <- getPaths
    purgeOld paths
    existing <- resolveRun paths c.run
    -- --run で指した実行が無ければ、黙って新しく作らない
    when (isJust c.run && isNothing existing) (usage "--run で指した実行が無い")
    (_, dir) <- maybe (newRun paths) pure existing
    f <- newFetcher paths.httpCache c.fresh (Just (dir </> "pages"))
    pref <- getPref
    r <- readItemShipped f pref url saved
    case r of
      Left (ItemUsage e) -> hPutStrLn stderr ("meguri: " <> e) >> exitWith (ExitFailure 2)
      Left (ItemFetch e) -> hPutStrLn stderr ("meguri: " <> e) >> exitWith (ExitFailure 3)
      Right rec -> do
        appendLines (dir </> "items.jsonl") [line "meguri.item/1" rec]
        case c.format of
          FJsonl -> BC.putStrLn (line "meguri.item/1" rec)
          _ -> TIO.putStr (itemReport rec)
  CmdRender c limit lo hi -> do
    paths <- getPaths
    existing <- resolveRun paths c.run
    case existing of
      Nothing -> hPutStrLn stderr ("meguri: 実行が無い。先に meguri search を走らせる" :: Text) >> exitWith (ExitFailure 2)
      Just (_, dir) -> do
        items <- readLines (dir </> "items.jsonl") :: IO [ItemRecord]
        cands <- readLines (dir </> "candidates.jsonl") :: IO [Candidate]
        cov <- readLines (dir </> "coverage.jsonl") :: IO [Coverage]
        TIO.putStr (renderMd limit (rank items (filter (inRange lo hi) cands)) cov)
  CmdCheck c write everything only -> do
    paths <- getPaths
    purgeOld paths
    -- 診断は、いつも取り直す（キャッシュを使わない）
    f <- newFetcher paths.httpCache True Nothing
    results <- Check.checkAll f (Check.CheckOpts everything only)
    case c.format of
      FJsonl -> traverse_ (\h -> putStrLn (T.intercalate "\t" [h.sid, h.day, h.search, h.item, show h.asExpected])) results
      _ -> TIO.putStr (Check.report results)
    when write $ do
      inRepo <- (&&) <$> doesFileExist "meguri.cabal" <*> doesFileExist "src/Meguri/Catalog/Health.hs"
      if not inRepo
        then usage "--write は、meguri のリポジトリの中で走らせる（meguri.cabal のあるフォルダ）"
        else do
          -- 一部のサイトだけ診断したときは、ほかのサイトの記録を残す。並びはカタログの順
          onDisk <- parseModule <$> TIO.readFile "src/Meguri/Catalog/Health.hs"
          let merged = merge results (if null onDisk then recorded else onDisk)
              ordered = [h | s <- catalog, h <- merged, h.sid == s.sid]
          TIO.writeFile "src/Meguri/Catalog/Health.hs" (moduleSource ordered)
          TIO.writeFile "docs/channels.md" (sitesMd ordered)
          hPutStrLn stderr ("meguri: src/Meguri/Catalog/Health.hs と docs/channels.md に書いた。ビルドし直すと、meguri sites にも入る" :: Text)
  CmdShip c url -> do
    pref <- getPref
    case (pref, pref >>= (`shipPlan` url)) of
      (Nothing, _) -> usage "MEGURI_PREF（配送先の都道府県コード。例：13）が設定されていない"
      (_, Nothing) -> usage ("このサイトは送料を計算できない（対応：" <> T.intercalate "・" supported <> "）")
      _ -> do
        paths <- getPaths
        existing <- resolveRun paths c.run
        (_, dir) <- maybe (newRun paths) pure existing
        f <- newFetcher paths.httpCache c.fresh (Just (dir </> "pages"))
        r <- readItemShipped f pref url Nothing
        case r of
          Left (ItemUsage e) -> usage e
          Left (ItemFetch e) -> hPutStrLn stderr ("meguri: " <> e) >> exitWith (ExitFailure 3)
          Right rec -> do
            appendLines (dir </> "items.jsonl") [line "meguri.item/1" rec]
            case c.format of
              FJsonl -> BC.putStrLn (line "meguri.item/1" rec)
              _ -> TIO.putStr (itemReport rec)
            -- 送料が計算の結果に置き換わっていなければ、問い合わせに失敗している
            case rec.offer.shipping.source of
              ItemPage ShippingCalc -> pure ()
              _ -> hPutStrLn stderr ("meguri: 送料の問い合わせに失敗した（記録の送料はページの値のまま）" :: Text) >> exitWith (ExitFailure 3)
  CmdRecall c qs limit -> do
    paths <- getPaths
    existing <- resolveRun paths c.run
    (rid, dir) <- maybe (newRun paths) pure existing
    f <- newFetcher paths.httpCache c.fresh (Just (dir </> "pages"))
    results <- for qs $ \q -> do
      caa <- fetchReq f (Recall.caaRequest q)
      cpsc <- fetchPage f (Recall.cpscUrl q)
      now <- jstStamp <$> getCurrentTime
      let text p = snd (decodeBody p.contentType p.body)
          one site method page parseOk = case page of
            Left e -> (Coverage site q New (Error e) Nothing method now, [])
            Right p -> case classify p.status (text p) of
              PageOk ->
                let rs = parseOk (jstStamp p.fetchedAt) (text p)
                 in (Coverage site q New (if null rs then Zero else Found (length rs) Nothing) Nothing method (jstStamp p.fetchedAt), rs)
              PageBlocked why -> (Coverage site q New (Blocked why) Nothing method now, [])
              PageStopped why -> (Coverage site q New (Stopped (T.take 10 now) why) Nothing method now, [])
              PageNotFound -> (Coverage site q New (Error "HTTP 404") Nothing method now, [])
              PageError why -> (Coverage site q New (Error why) Nothing method now, [])
      pure
        [ one "recall-caa" "HTTP（読み取り専用のPOST）" caa (\stamp body -> Recall.parseCaa q stamp (parse body))
        , one "recall-cpsc" "HTTP（公式API）" cpsc (Recall.parseCpsc q)
        ]
    let cov = map fst (concat results)
        recalls = concatMap snd (concat results)
    appendLines (dir </> "coverage.jsonl") (map (line "meguri.coverage/1") cov)
    appendLines (dir </> "recalls.jsonl") (map (line "meguri.recall/1") recalls)
    case c.format of
      FJsonl -> traverse_ (BC.putStrLn . line "meguri.recall/1") recalls
      _ ->
        TIO.putStr . T.unlines $
          ("リコール情報" <> show (length recalls) <> "件" <> (if length recalls > limit then "のうち" <> show limit <> "件" else "") <> "　※題に語が入っているだけのものも混ざる。対象の型番・期間は、リンク先で確かめる")
            : "出どころ\t日付\t題\t分類・対象\tURL"
            : map Recall.recallLine (take limit recalls)
            <> ["", coverageSummary cov, "実行 " <> rid <> "：" <> toS dir <> "（recalls.jsonl）"]
    exitByCoverage cov
  CmdRank c limit lo hi -> withRun c $ \_ dir -> do
    items <- readLines (dir </> "items.jsonl") :: IO [ItemRecord]
    cands <- readLines (dir </> "candidates.jsonl") :: IO [Candidate]
    let entries = rank items (filter (inRange lo hi) cands)
    BS.writeFile (dir </> "ranked.jsonl") (BC.unlines (map (line "meguri.ranked/1") entries))
    case c.format of
      FJsonl -> traverse_ (BC.putStrLn . line "meguri.ranked/1") entries
      _ -> TIO.putStr (rankTable limit entries)
  CmdVerify c top lo hi -> withRun c $ \_ dir -> do
    paths <- getPaths
    items <- readLines (dir </> "items.jsonl") :: IO [ItemRecord]
    cands <- readLines (dir </> "candidates.jsonl") :: IO [Candidate]
    f <- newFetcher paths.httpCache c.fresh (Just (dir </> "pages"))
    pref <- getPref
    rows <- Verify.verifyAll f pref (Verify.targets top items (filter (inRange lo hi) cands))
    let recs = [rec | Verify.Row _ (Verify.Read rec) _ <- rows]
    appendLines (dir </> "items.jsonl") (map (line "meguri.item/1") recs)
    case c.format of
      FJsonl -> traverse_ (BC.putStrLn . line "meguri.item/1") recs
      _ -> TIO.putStr (Verify.report rows)
    -- 取りに行って、1つも読めなかったら 3
    let tried = [() | Verify.Row _ o _ <- rows, case o of Verify.NeedsHtml _ -> False; _ -> True]
    when (not (null tried) && null recs) (exitWith (ExitFailure 3))
 where
  inRange lo hi c = case c.offer.priceIncl.value <|> c.offer.priceExcl.value of
    Just (Yen p) -> maybe True (p >=) lo && maybe True (p <=) hi
    Nothing -> isNothing lo && isNothing hi
  save dir cov cands = do
    appendLines (dir </> "coverage.jsonl") (map (line "meguri.coverage/1") cov)
    appendLines (dir </> "candidates.jsonl") (map (line "meguri.candidate/1") cands)
  output c rid dir cov cands limit = case c.format of
    FJsonl -> traverse_ (BC.putStrLn . line "meguri.candidate/1") (rankCandidates cands)
    FMd -> TIO.putStr (renderMd limit (rank [] cands) cov)
    FTable -> do
      -- 標準出力は 30KB 以内に収める。収まらなければ、候補の数を減らす
      let foot =
            "実行 " <> rid <> "：" <> toS dir <> "（candidates.jsonl・coverage.jsonl を jq で読む。ブラウザで保存したHTMLは pages/ に置くと、実行と一緒に7日で消える）\n"
          body n = candidateTable n cands <> "\n" <> coverageSummary cov <> "\n" <> foot
          size = BC.length . encodeUtf8
          fits n = size (body n) <= maxStdout
          shown = find fits (takeWhile (>= 0) (iterate (subtract 5) limit))
      TIO.putStr $ case shown of
        Just n -> body n
        -- 検索語が多くて集計だけで収まらないときは、行を切って、続きは jsonl に任せる
        Nothing ->
          let budget = maxStdout - size foot - 200
              kept = map snd (takeWhile ((<= budget) . fst) (drop 1 (scanl (\(used, _) l -> (used + size l + 1, l)) (0, "") (T.lines (coverageSummary cov)))))
           in T.unlines kept <> "（長いので、ここで切った。全体は coverage.jsonl）\n\n" <> foot
  usage msg = hPutStrLn stderr ("meguri: " <> msg :: Text) >> exitWith (ExitFailure 2)
  withRun c act = do
    paths <- getPaths
    existing <- resolveRun paths c.run
    case existing of
      Nothing -> hPutStrLn stderr ("meguri: 実行が無い。先に meguri search を走らせる" :: Text) >> exitWith (ExitFailure 2)
      Just (rid, dir) -> act rid dir

-- | 配送先の都道府県（環境変数 MEGURI_PREF）。設定が無ければ 'Nothing'。読めない値なら、使い方の誤り。
getPref :: IO (Maybe Pref)
getPref = do
  raw <- lookupEnv "MEGURI_PREF"
  case raw of
    Nothing -> pure Nothing
    Just "" -> pure Nothing
    Just v -> case parsePref (toS v) of
      Just p -> pure (Just p)
      Nothing -> hPutStrLn stderr ("meguri: MEGURI_PREF は都道府県コード（01〜47）か都道府県名で書く" :: Text) >> exitWith (ExitFailure 2)

-- | 1回の search の標準出力の上限（バイト）
maxStdout :: Int
maxStdout = 30000

-- | 0：成功（一部のサイトがブロックでも成功）。3：取りに行った全サイトで失敗。
exitByCoverage :: [Coverage] -> IO ()
exitByCoverage cov =
  let tried = [c | c <- cov, "HTTP" `T.isPrefixOf` c.method, attempted c.status]
      ok = [c | c <- tried, succeeded c.status]
   in when (not (null tried) && null ok) (exitWith (ExitFailure 3))
 where
  attempted = const True
  succeeded = \case
    Found _ _ -> True
    Zero -> True
    _ -> False
