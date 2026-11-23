-- | meguri：購入調査の決まった作業を受け持つ CLI
module Main (main) where

import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Meguri.Catalog
import Meguri.Check qualified as Check
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KM
import Meguri.Extract.Html (parse)
import Meguri.Fetch.Http
import Meguri.Judge (asker, newJev)
import Meguri.Query (Query (..), Seen)
import Meguri.Query qualified as Query
import Meguri.Health (Health (..), merge, moduleSource, parseModule, recorded)
import Meguri.Item
import Meguri.Search qualified as Search
import Meguri.Recall qualified as Recall
import Meguri.Render
import Meguri.Report qualified as Report
import Meguri.Run
import Meguri.Scope (Scope (..))
import Meguri.Scope qualified as Scope
import Meguri.Selection (Ask, Criterion, Judge (..), JudgeBy (..), Selected (..), Selection (..), select)
import Meguri.List
import Meguri.Ship (Pref, parsePref, shipPlan, supported)
import Meguri.Types
import Meguri.Read (Checked (..), Page (..))
import Options.Applicative
import Protolude
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hSetEncoding, utf8)

data Format = FTable | FJsonl | FMd
  deriving stock (Eq, Show)

newtype Common = Common
  { format :: Format
  }

data Command
  = CmdSites Common
  | -- | 枠ごとに標準出力に出す数つき
    CmdSearch Common Search.Request Int
  | CmdSold Common [Text] Int (Selection Seen)
  | CmdItem Common Text
  | CmdShip Common Text
  | CmdCheck Common Bool Bool [Text]
  | CmdRecall Common [Text] Int

common :: Parser Common
common =
  Common
    <$> option
      (eitherReader readFormat)
      (long "format" <> metavar "table|jsonl|md" <> value FTable <> help "出力の形（既定は table）")
 where
  readFormat = \case
    "table" -> Right FTable
    "jsonl" -> Right FJsonl
    "md" -> Right FMd
    other -> Left ("format は table・jsonl・md のどれか：" <> other)

commaList :: Mod OptionFields Text -> Parser [Text]
commaList m = concatMap (filter (not . T.null) . T.splitOn ",") <$> many (strOption m)

-- | 選び方の部品1つ（条件か判定）
type Part a = Either (Criterion a) (Judge a)

selection :: [Part a] -> Selection a
selection parts = Selection (lefts parts) (rights parts)

-- | どこを：当たる販路の選び方。何個でも書けて、全部を満たす販路に当たる
channelP :: Parser (Part Channel)
channelP =
  asum
    [ Left . Scope.only <$> channelList (long "only" <> metavar "SITE,..." <> help "どこを：この販路だけ当たる（id をカンマ区切りで）")
    , Left . Scope.skip <$> channelList (long "skip" <> metavar "SITE,..." <> help "どこを：この販路を除く（id をカンマ区切りで）")
    , Left . Scope.categoryHas <$> wordList (long "category" <> metavar "区分,..." <> help "どこを：区分の名前にどれかの語を含む販路だけ当たる（例：リユース,フリマ）")
    , Right . Scope.judgeByJev
        <$> strOption
          ( long "judge-channel"
              <> metavar "質問"
              <> help "どこを：当たる販路かどうかを、判定モデル jev に聞く（はい／いいえの質問。例：「この店はヘッドホンを扱うか」）。OPENROUTER_API_KEY が要る"
          )
    , Right . Scope.judgeByCommand
        <$> strOption
          ( long "judge-channel-cmd"
              <> metavar "コマンド"
              <> help "どこを：当たる販路かどうかを、外のコマンドに聞く。販路を JSON Lines で標準入力に渡し、当たる販路（id か、id を持つ JSON を1行に1つ）を標準出力で受け取る"
          )
    ]
 where
  split = filter (not . T.null) . map T.strip . T.splitOn "," . toS
  wordList m = split <$> strOption m
  -- 書きまちがいを、0件の結果にしない
  channelList = option . eitherReader $ \s -> case Scope.unknownChannels (split s) of
    [] -> Right (split s)
    bad -> Left (toS ("カタログに無い販路の id：" <> T.intercalate "・" bad <> "（meguri sites で一覧を見る）"))

-- | 何を：見つけた売り物のうち、探している物と認める選び方。何個でも書けて、全部を満たすものを認める
acceptP :: Parser (Part Seen)
acceptP =
  asum
    [ Left . Query.minPrice <$> option auto (long "min-price" <> metavar "円" <> help "何を：この額以上のもの（付属品よけ）")
    , Left . Query.maxPrice <$> option auto (long "max-price" <> metavar "円" <> help "何を：この額以下のもの")
    , Left . Query.nameHas <$> strOption (long "has" <> metavar "語" <> help "何を：商品名にこの語を含むもの")
    , Left . Query.nameLacks <$> strOption (long "not" <> metavar "語" <> help "何を：商品名にこの語を含まないもの")
    , Right . Query.judgeByJev
        <$> strOption
          ( long "judge"
              <> metavar "質問"
              <> help "何を：探している物かどうかを、判定モデル jev に聞く（はい／いいえの質問。例：「これはヘッドホン本体か（付属品ではないか）」）。OPENROUTER_API_KEY が要る"
          )
    , Right . Query.judgeByCommand
        <$> strOption
          ( long "judge-cmd"
              <> metavar "コマンド"
              <> help "何を：探している物かどうかを、外のコマンドに聞く。売り物を JSON Lines で標準入力に渡し、認めるもの（id か、id を持つ JSON を1行に1つ）を標準出力で受け取る"
          )
    ]

searchP :: Parser Command
searchP =
  ( \c terms markets parts reads urls secs limit ->
      let pages = [Page u Nothing | u <- urls]
       in CmdSearch
            c
            Search.Request
              { query = Query terms (selection (rights parts))
              , scope = Scope (selection (lefts parts)) markets pages
              , effort = Search.Effort reads secs
              }
            limit
  )
    <$> common
    <*> some (strArgument (metavar "語..." <> help "検索語（型番・JAN・商品名など）。複数あれば結果をまとめる"))
    <*> option
      (eitherReader readMarket)
      (long "market" <> metavar "new|used|all" <> value [] <> help "どこを：新品・中古・両方（既定は all）")
    -- 「どこを」と「何を」の選び方は、どの順に混ぜて書いてもよい
    <*> many ((Left <$> channelP) <|> (Right <$> acceptP))
    <*> option auto (long "read" <> metavar "N" <> value 3 <> help "どこまで：探している物と認めたもののうち、販路と新品・中古の組ごとに安い順で N 件の個別ページを読む（既定は 3。0 なら一覧だけ）")
    <*> many (strOption (long "url" <> metavar "URL" <> help "どこを：この個別ページも読んで、結果に入れる（検索がJSで描画される販路で、ブラウザで見つけた商品など）"))
    <*> option auto (long "time" <> metavar "秒" <> value 60 <> help "どこまで：全体にかける時間の上限（既定は 60）。過ぎたら、読めた分だけで返し、読めなかったものを「時間切れ」として出す")
    <*> option auto (long "limit" <> metavar "N" <> value 15 <> help "枠ごとに標準出力に出す数（既定は 15）")
 where
  readMarket = \case
    "new" -> Right [New]
    "used" -> Right [Used]
    "all" -> Right []
    other -> Left ("market は new・used・all のどれか：" <> other)

commands :: Parser Command
commands =
  hsubparser $
    mconcat
      [ command "search" (info searchP (progDesc "選んだ販路を並列に当たり、探している物と認めたものの個別ページ（購入ページ）まで読んで、読めた項目と読んだ時刻を返す"))
      , command "item" (info itemP (progDesc "個別ページを1つ読み、基準の項目を出どころつきで出す"))
      , command "sold" (info soldP (progDesc "売れた価格を出す（ヤフオクの落札相場）"))
      , command "recall" (info recallP (progDesc "消費者庁と CPSC のリコール情報を出す"))
      , command "ship" (info shipP (progDesc "送料を計算できるサイト（ハードオフ・トレジャーファクトリー）で、MEGURI_PREF の配送先までの送料を出す"))
      , command "sites" (info (CmdSites <$> common) (progDesc "カタログ（販路の一覧）を出す。--format md で channels.md を作る"))
      , command "check" (info checkP (progDesc "サイトごとに健康診断をする（検索1回・個別1件）。--write で、結果と日付をカタログと docs/channels.md に書く"))
      ]
 where
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
  itemP =
    CmdItem
      <$> common
      <*> strArgument (metavar "URL" <> help "個別ページのURL")
  soldP =
    (\c qs n parts -> CmdSold c qs n (selection parts))
      <$> common
      <*> some (strArgument (metavar "語..."))
      <*> option auto (long "limit" <> metavar "N" <> value 40 <> help "標準出力に出す件数（既定は 40）")
      <*> many acceptP

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  let parser = info (commands <**> helper) (fullDesc <> progDesc "購入調査の決まった作業（検索・選び方・個別ページの確認・表）を受け持つ。買わない・ログインしない。")
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
  CmdSearch c req limit -> do
    needKey (map (.by) req.query.accept.judges <> map (.by) req.scope.channels.judges)
    paths <- getPaths
    purgeOld paths
    (rid, dir) <- newRun paths
    f <- newFetcher paths.locksDir (Just (dir </> "pages"))
    pref <- getPref
    ask <- getAsk
    r <- Search.search ask f pref req
    case r of
      Left e -> judgeFailed e
      Right res -> do
        -- 実行の記録（書くだけ。あとのコマンドは読まない）
        writeLines (dir </> "coverage.jsonl") (map (line "meguri.coverage/3") res.coverage)
        writeLines (dir </> "sightings.jsonl") (map (line "meguri.sighting/2") res.sightings)
        writeLines (dir </> "readings.jsonl") [line "meguri.reading/2" (withAttempts x.reading x.attempts) | x <- res.checked]
        let foot = "実行の記録 " <> rid <> "：" <> toS dir <> "（readings.jsonl＝読んだ個別ページの全項目と出どころ、sightings.jsonl＝一覧で見かけた売り物の全部、coverage.jsonl、pages/＝取ったページ。7日で消える）\n"
        case c.format of
          FJsonl -> traverse_ BC.putStrLn (Report.jsonLines res)
          FMd -> TIO.putStr (Report.markdown 40 res)
          FTable -> TIO.putStr (fitted (\n -> Report.table req.effort.readsPerLookup n res <> "\n" <> foot) limit)
        exitByCoverage res.coverage
  CmdSold c qs limit accept -> do
    needKey (map (.by) accept.judges)
    paths <- getPaths
    purgeOld paths
    f <- newFetcher paths.locksDir Nothing
    let closed = "https://auctions.yahoo.co.jp/closedsearch/closedsearch?p={q}"
        -- 英字で終わる型番は、英字を外した語でも探す（新しい順に100件まで）。題にもとの語を含むものだけを足す
        wider def (w, has) = Wider w (searchUrl def.enc (closed <> "&n=100") w) has
        jobs =
          [ Lookup def q Used (Just (searchUrl def.enc closed q)) Nothing (wider def <$> widen q)
          | Just def <- [lookupSite "yahuoku"]
          , q <- qs
          ]
    (cov, sold) <- listAll f jobs
    -- 語を広げた検索が取れたときの但し書き
    let widened = T.unlines ["語を広げた検索は、新しい順に100件までを見た" | or [fine a | c <- cov, a <- attemptsOf c.did, WiderPage _ _ <- [a.role]]]
    ask <- getAsk
    selected <- select ask Query.seenRows accept Query.ofSighting sold
    case selected of
      Left e -> judgeFailed e
      Right shown -> case c.format of
        FJsonl -> traverse_ (BC.putStrLn . line "meguri.sold/2") (cheapestFirst shown.chosen)
        _ -> TIO.putStr (soldTable limit shown.chosen <> "\n" <> coverageSummary cov <> widened)
    exitByCoverage cov
  CmdItem c url -> do
    paths <- getPaths
    f <- newFetcher paths.locksDir Nothing
    pref <- getPref
    readItemShipped f Nothing pref url >>= \case
      ItemRead rec as -> printItem c rec as
      other -> itemFailed other
  CmdCheck c write everything only -> do
    paths <- getPaths
    purgeOld paths
    f <- newFetcher paths.locksDir Nothing
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
        f <- newFetcher paths.locksDir Nothing
        r <- readItemShipped f Nothing pref url
        case r of
          ItemRead rec as -> do
            printItem c rec as
            -- 送料が計算の結果に置き換わっていなければ、問い合わせがうまくいっていない
            case rec.offer.shipping.source of
              ItemPage ShippingCalc -> pure ()
              _ -> do
                let why = maybe "応答から送料を読めなかった" troubleShort (find (\a -> a.role == ShipQuery) (troubles as))
                hPutStrLn stderr ("meguri: 送料の問い合わせがうまくいかなかった（記録の送料はページの値のまま）。" <> why)
                exitWith (ExitFailure 3)
          other -> itemFailed other
  CmdRecall c qs limit -> do
    paths <- getPaths
    f <- newFetcher paths.locksDir Nothing
    results <- for qs $ \q -> do
      let one site method req parseOk = do
            (a, loaded) <- attempt f Nothing SearchPage req
            -- 読むのは、読めるページが返ったときだけ
            let rs = [x | a.outcome == Got, Just p <- [loaded], x <- parseOk p.at p.text]
                listed = [Listed {offers = length rs, zero = null rs, kept = length rs} | a.outcome == Got]
            pure (Coverage {site, term = q, market = New, url = Nothing, did = Tried [a {listed = head listed}], cheapest = Nothing, method, at = a.at}, rs)
      caa <- one "recall-caa" "HTTP（読み取り専用のPOST）" (Recall.caaRequest q) (\stamp body -> Recall.parseCaa q stamp (parse body))
      cpsc <- one "recall-cpsc" "HTTP（公式API）" (page (Recall.cpscUrl q)) (Recall.parseCpsc q)
      pure [caa, cpsc]
    let cov = map fst (concat results)
        recalls = concatMap snd (concat results)
    case c.format of
      FJsonl -> traverse_ (BC.putStrLn . line "meguri.recall/1") recalls
      _ ->
        TIO.putStr . T.unlines $
          ("リコール情報" <> show (length recalls) <> "件" <> (if length recalls > limit then "のうち" <> show limit <> "件" else "") <> "　※題に語が入っているだけのものも混ざる。対象の型番・期間は、リンク先で確かめる")
            : "出どころ\t日付\t題\t分類・対象\tURL"
            : map Recall.recallLine (take limit recalls)
            <> ["", coverageSummary cov]
    exitByCoverage cov
 where
  -- 取得の並びも出す。表には、うまくいかなかった取得だけを足す
  printItem c rec as = case c.format of
    FJsonl -> BC.putStrLn (line "meguri.item/2" (withAttempts rec as))
    _ -> TIO.putStr (readingReport rec <> T.unlines ["うまくいかなかった取得\t" <> troubleNote a <> "\t" <> a.url | a <- troubles as])
  usage :: Text -> IO a
  usage msg = hPutStrLn stderr ("meguri: " <> msg) >> exitWith (ExitFailure 2)
  -- 取りに行かない販路は使い方の誤り（2）。取りに行って読めなかったら 3
  itemFailed :: ItemResult -> IO a
  itemFailed = \case
    ItemSkipped why -> usage (skipShort why)
    ItemUnread as -> do
      for_ (troubles as) $ \a -> hPutStrLn stderr ("meguri: " <> troubleNote a <> "　" <> a.url)
      exitWith (ExitFailure 3)
    ItemRead _ _ -> exitSuccess
  -- 標準出力は 30KB 以内に収める。収まらなければ、枠ごとの行数を減らす。
  -- それでも収まらなければ（検索語が多いとき）、行を切って、続きは実行の記録に任せる
  fitted :: (Int -> Text) -> Int -> Text
  fitted body limit =
    let size = BC.length . encodeUtf8
     in case find ((<= maxStdout) . size . body) (takeWhile (> 0) (iterate (subtract 2) limit)) of
          Just n -> body n
          Nothing ->
            let ls = T.lines (body 1)
                kept = map snd (takeWhile ((<= maxStdout - 300) . fst) (drop 1 (scanl (\(used, _) l -> (used + size l + 1, l)) (0, "") ls)))
             in T.unlines kept <> "（長いので、ここで切った。全体は、実行の記録の coverage.jsonl・readings.jsonl）\n" <> fromMaybe "" (lastMay ls) <> "\n"

-- | 判定の頼み先。jev は、OPENROUTER_API_KEY があるときだけ使える（モデルは MEGURI_JEV_MODEL で変えられる）
getAsk :: IO Ask
getAsk = do
  key <- lookupEnv "OPENROUTER_API_KEY"
  model <- lookupEnv "MEGURI_JEV_MODEL"
  case key of
    Just k | not (null k) -> asker . Just <$> newJev (toS k) (toS <$> mfilter (not . null) model)
    _ -> pure (asker Nothing)

-- | jev に聞く判定があるのにキーが無ければ、取りに行く前にやめる
needKey :: [JudgeBy] -> IO ()
needKey bys = do
  key <- lookupEnv "OPENROUTER_API_KEY"
  when (any isJev bys && maybe True null key) $ do
    hPutStrLn stderr ("meguri: --judge・--judge-channel は、判定モデル jev を OpenRouter 経由で呼ぶ。環境変数 OPENROUTER_API_KEY が要る（外のコマンドに任せるなら --judge-cmd・--judge-channel-cmd）" :: Text)
    exitWith (ExitFailure 2)
 where
  isJev = \case
    Jev _ -> True
    Command _ -> False

judgeFailed :: Text -> IO a
judgeFailed e = hPutStrLn stderr ("meguri: 判定が失敗したので、やめた。" <> e) >> exitWith (ExitFailure 3)

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

-- | 記録に、取得の並び（attempts）を足す
withAttempts :: Reading -> [Attempt] -> Value
withAttempts rec as = case toJSON rec of
  Object o -> Object (KM.insert "attempts" (toJSON as) o)
  v -> v

-- | 0：成功（一部のサイトがブロックでも成功）。3：取りに行った全サイトで失敗。
exitByCoverage :: [Coverage] -> IO ()
exitByCoverage cov =
  let tried = [statusOf c | c <- cov, Tried _ <- [c.did]]
      ok = [st | st <- tried, succeeded st]
   in when (not (null tried) && null ok) (exitWith (ExitFailure 3))
 where
  succeeded = \case
    Found _ _ -> True
    Zero -> True
    _ -> False
