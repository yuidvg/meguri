module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (ToJSON, Value (Object), object, toJSON, (.=))
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty')
import Data.Aeson (decodeStrict')
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Set qualified as Set
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, diffUTCTime, fromGregorian, getCurrentTime)
import Hedgehog (forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Meguri.Catalog
import Meguri.Check (itemLinks)
import Meguri.Health (Health (..), merge, moduleSource, parseModule)
import Meguri.Confirm
import Meguri.Extract.Html (parse)
import Meguri.Extract.Microdata (itemprop)
import Meguri.Extract.Price
import Meguri.Fetch.Encoding
import Meguri.Fetch.Http (Fetched (..), PageClass (..), blockOf, blockSign, classify, newFetcher, takeTurn)
import Meguri.Item (extractItem)
import Meguri.Arrange (Frame (..), arrange)
import Meguri.Recall qualified as Recall
import Meguri.Ship (Pref (..), ShipPlan (..), parsePref, shipPlan)
import Meguri.Fetch.Http (Req (..))
import Meguri.Arrange qualified as Rank
import Meguri.Judge (asker, commandInput, commandKept, jevAnswer, jevKeeps, jevRequest)
import Meguri.Query (judgeByCommand, judgeByJev, maxPrice, minPrice, nameHas, nameLacks, ofSighting, seenRows)
import Meguri.Scope (Scope (..), categoryHas, channelRows, channelsIn, only, skip, unknownChannels)
import Meguri.Scope qualified as Scope
import Meguri.Selection (Ask, Excluded (..), Selected (..), Selection (..), everything, select, sift)
import Meguri.Search qualified as Search
import Meguri.Report qualified as Report
import Meguri.Read (Checked (..), Page (..), Unread (..), UnreadReason (..), cheapest, driftFrom, tracked)
import Meguri.Render (sightingLine, coverageSummary, cheapestFirst, sitesMd)
import Meguri.Run (Paths (..), newRun, purgeOld)
import Meguri.Sanitize (publicUrl, tidyOffer, usableUrl)
import Meguri.List
import Meguri.Site.Common
import Meguri.Site.Store (taxInclAmount, validEan13)
import Meguri.Site.Yahuoku qualified as Yahuoku
import Meguri.Sites
import Meguri.Text
import Meguri.Types
import Protolude
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory, setModificationTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain $
    testGroup
      "meguri"
      [ goldenTests
      , priceTests
      , shippingTests
      , encodingTests
      , confirmTests
      , rankTests
      , shipTests
      , storeTests
      , scopeTests
      , acceptTests
      , searchTests
      , widenTests
      , fetchTests
      , concurrencyTests
      , microdataTests
      , sanitizeTests
      , catalogTests
      , checkTests
      ]

stamp :: Text
stamp = "2026-11-20T23:00:00+09:00"

pretty :: ToJSON a => a -> BL.ByteString
pretty a = encodePretty' defConfig {confCompare = compare} a <> "\n"

readFixture :: FilePath -> IO Text
readFixture path = snd . decodeBody Nothing <$> BS.readFile ("test/fixtures/" <> path)

-- | 一覧の fixture → 候補
listGolden :: Text -> FilePath -> Text -> Int -> TestTree
listGolden siteId file url status =
  goldenVsString (toS siteId <> "/" <> file) ("test/golden/" <> toS siteId <> "/" <> file <> ".json") $ do
    html <- readFixture (toS siteId <> "/" <> file <> ".html")
    let def = fromMaybe (panic "カタログに無い") (lookupSite siteId)
        ex = fromMaybe (panic "一覧のアダプタが無い") (adapterFor def).listX
        out = ex (ListCtx siteId "WH-1000XM5" Used url status stamp) (parse html) html
    pure (pretty (object ["offers" .= out.offers, "zero" .= out.zero, "follow" .= out.follow]))

-- | 個別ページの fixture → 記録（判定つき）
itemGolden :: Text -> FilePath -> Text -> TestTree
itemGolden siteId file url =
  goldenVsString (toS siteId <> "/" <> file) ("test/golden/" <> toS siteId <> "/" <> file <> ".json") $ do
    html <- readFixture (toS siteId <> "/" <> file <> ".html")
    pure (pretty (extractItem url stamp html))

goldenTests :: TestTree
goldenTests =
  testGroup
    "golden（fixture からの抽出）"
    [ listGolden "kakaku" "search" "https://kakaku.com/search_results/WH-1000XM5/" 200
    , listGolden "kakaku" "shop" "https://kakaku.com/item/K0001654211/shop/" 200
    , listGolden "kakaku" "search-jan" "https://kakaku.com/search_results/4548736132566/" 200
    , listGolden "rakuten" "search" "https://search.rakuten.co.jp/search/mall/WH-1000XM5/?s=2&used=1" 200
    , itemGolden "rakuten" "item" "https://item.rakuten.co.jp/cocohouse/nb08qd1ckcd/"
    , itemGolden "rakuten" "item-2ndstreet" "https://item.rakuten.co.jp/jumblestore/2238010018076/"
    , listGolden "yahoo-shopping" "search" "https://shopping.yahoo.co.jp/search?p=WH-1000XM5&X=2&used=1" 200
    , listGolden "yahoo-shopping" "zero" "https://shopping.yahoo.co.jp/search?p=NA-LX127C&X=2&used=1" 200
    , itemGolden "yahoo-shopping" "item" "https://store.shopping.yahoo.co.jp/eco-sty/503763141.html"
    , listGolden "yahuoku" "search" "https://auctions.yahoo.co.jp/search/search?p=WH-1000XM5&s1=cbids&o1=a" 200
    , listGolden "yahuoku" "search-flea" "https://auctions.yahoo.co.jp/search/search?p=NA-LX127CR&s1=cbids&o1=a" 200
    , listGolden "yahuoku" "closedsearch" "https://auctions.yahoo.co.jp/closedsearch/closedsearch?p=WH-1000XM5" 200
    , itemGolden "yahuoku" "item" "https://auctions.yahoo.co.jp/jp/auction/f1252794818"
    , listGolden "paypayfleamarket" "search" "https://paypayfleamarket.yahoo.co.jp/search/WH-1000XM5?open=1&sort=price&order=asc" 200
    , listGolden "paypayfleamarket" "search-near" "https://paypayfleamarket.yahoo.co.jp/search/NA-LX127C?open=1&sort=price&order=asc" 404
    , itemGolden "paypayfleamarket" "item" "https://paypayfleamarket.yahoo.co.jp/item/z539816586"
    , itemGolden "paypayfleamarket" "item-app" "https://paypayfleamarket.yahoo.co.jp/item/z712208474"
    , itemGolden "paypayfleamarket" "item-sold" "https://paypayfleamarket.yahoo.co.jp/item/z675193022"
    , listGolden "rakuma" "search" "https://fril.jp/s?query=WH-1000XM5&transaction=selling&sort=sell_price&order=asc" 200
    , itemGolden "rakuma" "item" "https://item.fril.jp/b482bbfa475809f13059c615705c3340"
    , listGolden "hardoff" "search" "https://netmall.hardoff.co.jp/search/?q=WH-1000XM5&s=3" 200
    , itemGolden "hardoff" "item" "https://netmall.hardoff.co.jp/product/6220014/"
    , listGolden "treasure-factory" "search" "https://ec.treasure-f.com/search?word=WH-1000XM5&order=lowprice&step=1" 200
    , itemGolden "treasure-factory" "item" "https://ec.treasure-f.com/item/2503005450576041"
    , listGolden "jmty" "search" "https://jmty.jp/all/sale/search?keyword=WH-1000XM5" 200
    , itemGolden "jmty" "item" "https://jmty.jp/tokyo/sale-ele/article-1tcsaa"
    , listGolden "bookoff" "search" "https://shopping.bookoff.co.jp/search/keyword/WH-1000XM5?sort=50" 200
    , itemGolden "bookoff" "item" "https://shopping.bookoff.co.jp/used/0012651925"
    , listGolden "loft" "search" "https://www.loft.co.jp/store/goods/search.aspx?search=x&keyword=%E3%82%BF%E3%82%AA%E3%83%AB&sort=price" 200
    , itemGolden "loft" "item" "https://www.loft.co.jp/store/g/g4902011649215/"
    , itemGolden "francfranc" "item" "https://francfranc.com/products/1111070077126"
    , listGolden "kohnan" "search" "https://www.kohnan-eshop.com/shop/goods/search.aspx?keyword=%E8%84%9A%E7%AB%8B&search=x&seq=gp" 200
    , itemGolden "kohnan" "item" "https://www.kohnan-eshop.com/shop/g/g4989999212587/"
    , listGolden "dinos" "search" "https://www.dinos.co.jp/defaultMall/sitemap/CSfSearchList_001.jsp?KEY_SEARCH=%83%5C%83t%83%40&sort=1" 200
    , itemGolden "dinos" "item" "https://www.dinos.co.jp/p/1137106226/"
    , listGolden "japanet" "search" "https://www.japanet.co.jp/shopping/swd/%E6%8E%83%E9%99%A4%E6%A9%9F/" 200
    , itemGolden "japanet" "item" "https://www.japanet.co.jp/shopping/keyword/catdetail/ATINNV0950LP000000000010001/PDW0E1500013B/CS6300005535863103/DS63003055358/"
    , listGolden "iosys" "search" "https://iosys.co.jp/items?q=iPhone%2013&sort=l" 200
    , itemGolden "iosys" "item" "https://iosys.co.jp/items/smartphonegoods/other/%E3%82%AC%E3%83%A9%E3%82%B9%E3%83%95%E3%82%A3%E3%83%AB%E3%83%A0_for_iphone13_mini/224665"
    , listGolden "cainz" "search" "https://www.cainz.com/search/?q=%E8%84%9A%E7%AB%8B&sortRule=lowPrice" 200
    , itemGolden "cainz" "item" "https://www.cainz.com/g/4571509331095.html"
    , itemGolden "costco" "item" "https://www.costco.co.jp/c/Kleenex-Tissue-180ct-x-10-Boxes/p/515685"
    , itemGolden "ikea" "item" "https://www.ikea.com/jp/ja/p/kuddarna-chair-cushion-outdoor-light-grey-beige-80547223/"
    , itemGolden "sofmap" "item" "https://www.sofmap.com/product_detail.aspx?sku=23301270"
    , listGolden "hands" "search" "https://hands.net/search/?q=%E3%82%BF%E3%82%AA%E3%83%AB&rankBy=price(float)/jpy:ascending" 200
    , listGolden "hands" "search-zero" "https://hands.net/search/?q=WH-1000XM5&rankBy=price(float)/jpy:ascending" 200
    , itemGolden "hands" "item" "https://hands.net/goods/4517161163230/"
    , listGolden "ishibashi" "search" "https://store.ishibashi.co.jp/search?q=%E3%82%AE%E3%82%BF%E3%83%BC&sort=price" 200
    , listGolden "ishibashi" "search-zero" "https://store.ishibashi.co.jp/search?q=WH-1000XM5&sort=price" 200
    , itemGolden "ishibashi" "item" "https://store.ishibashi.co.jp/view/item/000000040360"
    , listGolden "buyma" "search" "https://www.buyma.com/r/-O3/%E8%B2%A1%E5%B8%83/" 200
    , listGolden "buyma" "search-zero" "https://www.buyma.com/r/-O3/WH-1000XM5/" 404
    , itemGolden "buyma" "item" "https://www.buyma.com/item/120505097/"
    , itemGolden "lohaco" "item" "https://lohaco.yahoo.co.jp/store/h-lohaco/item/uu42216/"
    , itemGolden "aupay" "item" "https://wowma.jp/item/745282614"
    , listGolden "cado" "search" "https://cado.com/search?q=%E5%8A%A0%E6%B9%BF%E5%99%A8" 200
    , itemGolden "cado" "item" "https://cado.com/products/hm-c500h"
    , listGolden "okoku" "search" "https://www.okoku.jp/search?q=%E3%83%98%E3%83%83%E3%83%89%E3%83%9B%E3%83%B3&sort_by=price-ascending&filter.v.availability=1" 200
    , itemGolden "okoku" "item" "https://www.okoku.jp/products/2100228144178"
    , listGolden "akachan" "search" "https://shop.akachan.jp/shop/goods/search.aspx?search=x&keyword=%E3%83%99%E3%83%93%E3%83%BC%E3%82%AB%E3%83%BC&sort=sp" 200
    , itemGolden "akachan" "item" "https://shop.akachan.jp/shop/g/g204781100/"
    , itemGolden "locondo" "item" "https://www.locondo.jp/shop/commodity/SCLR0682D/IN8133DU000020/"
    , goldenVsString "recall-caa/search" "test/golden/recall-caa/search.json" $ do
        html <- readFixture "recall-caa/search.html"
        pure (pretty (Recall.parseCaa "ヘッドホン" stamp (parse html)))
    , goldenVsString "recall-cpsc/search" "test/golden/recall-cpsc/search.json" $ do
        body <- readFixture "recall-cpsc/search.json"
        pure (pretty (Recall.parseCpsc "headphone" stamp body))
    ]

priceTests :: TestTree
priceTests =
  testGroup
    "価格の読み取り"
    [ testCase "¥1,980（税込）" $ parsePrice "¥1,980（税込）" @?= Just (Yen 1980, Just MarkIncl)
    , testCase "1,980円" $ parsePrice "1,980円" @?= Just (Yen 1980, Nothing)
    , testCase "全角：１，９８０円" $ parsePrice "１，９８０円" @?= Just (Yen 1980, Nothing)
    , testCase "税抜の表示" $ parsePrice "1,800円（税抜）" @?= Just (Yen 1800, Just MarkExcl)
    , testCase "本体価格＋税" $ taxMark "本体価格 1,800円+税" @?= Just MarkExcl
    , testCase "数字が無い" $ parseYen "価格未定" @?= Nothing
    , testCase "全角を半角に" $ toHalfWidth "ＷＨ－１０００ＸＭ５　（Ｂ）" @?= "WH-1000XM5 (B)"
    , testProperty "桁区切り・全角でも同じ額になる" $ property $ do
        n <- forAll (Gen.int (Range.linear 0 99999999))
        let plain = show n :: Text
            commas = T.intercalate "," (reverse (map T.reverse (T.chunksOf 3 (T.reverse plain))))
            wide = T.map (\c -> if c >= '0' && c <= '9' then toEnum (fromEnum c + 0xFEE0) else c) commas
        parseYen (commas <> "円") === Just (Yen n)
        parseYen ("￥" <> wide) === Just (Yen n)
    ]

shippingTests :: TestTree
shippingTests =
  testGroup
    "送料の読み取り・支払額"
    [ testCase "送料無料" $ parseShipping "送料無料" @?= Just Free
    , testCase "送料込" $ parseShipping "送料込" @?= Just Free
    , testCase "+送料550円" $ parseShipping "+送料550円" @?= Just (Fixed (Yen 550))
    , testCase "送料：1,100円" $ parseShipping "送料：1,100円（税込）" @?= Just (Fixed (Yen 1100))
    , testCase "条件つき無料" $ parseShipping "送料550円（3,300円以上で送料無料）" @?= Just (FreeOver (Yen 3300) (Yen 550))
    , testCase "条件だけ（額が無い）は作らない" $ parseShipping "3,300円以上で送料無料" @?= Just (ShipUnknown "3,300円以上で送料無料")
    , testCase "しきい値に（税込）が付いていても、条件つき無料として読む" $
        parseShipping "送料550円（3,300円（税込）以上で送料無料）" @?= Just (FreeOver (Yen 3300) (Yen 550))
    , testCase "空白で区切られた「3,780 円 (税込)以上で基本配送料 550 円 (税込) 無料」" $
        parseShipping "最短翌日お届け 3,780 円 (税込)以上で基本配送料 550 円 (税込) 無料" @?= Just (FreeOver (Yen 3780) (Yen 550))
    , testCase "「送料無料」の後ろのしきい値を、送料の額と取り違えない" $
        parseShipping "4,000円（税込）以上のご購入で送料無料。4,000円（税込）以下の場合は、送料は一律550円（税込）。" @?= Just (FreeOver (Yen 4000) (Yen 550))
    , testCase "着払い" $ parseShipping "送料着払い" @?= Just BuyerPays
    , testCase "落札者負担" $ parseShipping "送料：落札者負担" @?= Just BuyerPays
    , testCase "読めない" $ parseShipping "お届け日を確認" @?= Nothing
    , testCase "しきい値未満は送料がかかる" $ shippingCost (Yen 3000) (FreeOver (Yen 3300) (Yen 550)) @?= Just (Yen 550)
    , testCase "しきい値以上は無料" $ shippingCost (Yen 3300) (FreeOver (Yen 3300) (Yen 550)) @?= Just (Yen 0)
    , testCase "配送先しだいなら支払額は出さない" $ shippingCost (Yen 3300) ByDestination @?= Nothing
    ]

encodingTests :: TestTree
encodingTests =
  testGroup
    "文字コード"
    [ testCase "検索語を Shift_JIS で % エンコード" $
        searchUrl ShiftJis "https://example.test/?q={q}" "掃除機" @?= "https://example.test/?q=%91%7C%8F%9C%8B%40"
    , testCase "検索語を UTF-8 で % エンコード（空白と記号）" $
        searchUrl Utf8 "https://example.test/{q}/" "iPhone 13/黒" @?= "https://example.test/iPhone%2013%2F%E9%BB%92/"
    , testCase "cp932 の機種依存文字（①・髙）" $
        percentEncode (encodeQuery Cp932 "①髙") @?= "%87%40%FB%FC"
    , testCase "EUC-JP で % エンコード" $
        percentEncode (encodeQuery EucJp "時計") @?= "%BB%FE%B7%D7"
    , testCase "cp932 を読む（ヘッダ）" $
        decodeBody (Just "text/html; charset=Shift_JIS") (BS.pack [0x89, 0xBF, 0x8A, 0x69, 0x87, 0x40]) @?= (CsCp932, "価格①")
    , testCase "EUC-JP を読む（meta）" $
        snd (decodeBody Nothing ("<meta charset=\"EUC-JP\">" <> BS.pack [0xC1, 0xF7, 0xCE, 0xC1])) @?= "<meta charset=\"EUC-JP\">送料"
    , testCase "ヘッダが meta より先" $
        fst (decodeBody (Just "text/html;charset=Windows-31J") "<meta charset=\"utf-8\">") @?= CsCp932
    , testCase "どちらも無ければ中身で決める（UTF-8）" $
        decodeBody Nothing (encodeUtf8 "価格") @?= (CsUtf8, "価格")
    , testCase "どちらも無ければ中身で決める（cp932）" $
        decodeBody Nothing (BS.pack [0x89, 0xBF, 0x8A, 0x69]) @?= (CsCp932, "価格")
    ]

-- | 確認済みの判定（2.2 の規則）
confirmTests :: TestTree
confirmTests =
  testGroup
    "確認済みかどうかの判定"
    [ testCase "7項目すべて読めたら confirmed" $ judge True full @?= Confirmed
    , testCase "送料が読めなければ partial" $
        judge True full {shipping = missing (ItemPage Body) "無い"} @?= Partial ["送料"]
    , testCase "送料が「不明」でも partial" $
        judge True full {shipping = got (ItemPage Body) (ShipUnknown "相談")} @?= Partial ["送料"]
    , testCase "配送先しだいの送料は読めた扱い（支払額は null）" $ do
        let o = full {shipping = got (ItemPage Body) ByDestination}
        judge True o @?= Confirmed
        payable o @?= Nothing
    , testCase "税抜しか無ければ税込は null のまま → partial" $
        judge True full {priceIncl = missing (ItemPage JsonLd) "税抜の表示", priceExcl = got (ItemPage JsonLd) (Yen 1000)} @?= Partial ["税込価格"]
    , testCase "何も読めなければ unconfirmed" $
        judge True (emptyOffer (ItemPage Body) "x" "https://example.test/" stamp) @?= Unconfirmed
    , testCase "アダプタなし（汎用の抽出だけ）は confirmed にしない" $
        judge False full @?= Partial ["アダプタ（汎用の抽出だけ）"]
    , testCase "支払額＝税込価格＋送料" $ payable full @?= Just (Yen 2530)
    ]
 where
  draft :: Offer Method
  draft =
    (emptyOffer Body "x" "https://example.test/item/1" stamp)
      { name = got JsonLd "商品"
      , priceIncl = got JsonLd (Yen 1980)
      , shipping = got Body (Fixed (Yen 550))
      , stock = got JsonLd InStock
      , seller = got Body "店"
      , image = got OpenGraph "https://example.test/1.jpg"
      , buy = got Body (BuyActive "カートに入れる")
      }
  full :: Offer OnPage
  full = (toReading True draft).offer

-- | テスト用の候補（一覧の値）
mkCand :: Text -> Text -> Text -> Int -> Sighting
mkCand siteId url name price =
  Sighting "WH-1000XM5" New ((emptyOffer SearchList siteId url stamp) {name = got SearchList name, priceIncl = got SearchList (Yen price), shipping = got SearchList Free})

-- | テスト用の記録（個別ページで7項目すべて読めたもの）
mkItem :: Text -> Text -> Int -> Reading
mkItem url name price =
  toReading
    True
    (emptyOffer Body "x" url stamp)
      { name = got JsonLd name
      , priceIncl = got JsonLd (Yen price)
      , shipping = got Body Free
      , stock = got JsonLd InStock
      , seller = got Body "店"
      , image = got OpenGraph "https://example.test/1.jpg"
      , buy = got Body (BuyActive "カートに入れる")
      }

-- | 判定の頼み先（テスト用）：外のコマンドだけ。jev のキーは無い
noJev :: Ask
noJev = asker Nothing

-- | どこを探すか：当たる販路を、カタログから選ぶ
scopeTests :: TestTree
scopeTests =
  testGroup
    "どこを（Scope）"
    [ testCase "選び方が無ければ、カタログの全部に当たる" $ do
        r <- channelsIn noJev ["x"] (scopeOf everything)
        fmap (\c -> (length c.chosen, c.excluded)) r @?= Right (length catalog, [])
    , testCase "条件を全部満たす販路に当たる。どれがいくつ外したかを数える" $ do
        r <- channelsIn noJev ["x"] (scopeOf (Selection [categoryHas ["リユース", "フリマ"], skip ["hardoff"], only ["mercari", "bookoff", "rakuten"]] []))
        let inCat = length [c | c <- catalog, c.category `elem` [Reuse, FleaAuction]]
        fmap (\c -> (sort (map (.sid) c.chosen), c.excluded)) r
          @?= Right (["bookoff", "mercari"], [Excluded "--category リユース,フリマ" (length catalog - inCat), Excluded "--skip hardoff" 1, Excluded "--only mercari,bookoff,rakuten" (inCat - 3)])
    , testCase "条件の順を変えても、当たる販路は同じ" $ do
        let cs = [categoryHas ["リユース", "フリマ"], skip ["hardoff"], only ["mercari", "bookoff", "rakuten"]]
        a <- channelsIn noJev ["x"] (scopeOf (Selection cs []))
        b <- channelsIn noJev ["x"] (scopeOf (Selection (reverse cs) []))
        fmap (map (.sid) . (.chosen)) a @?= fmap (map (.sid) . (.chosen)) b
    , testCase "カタログに無い id は、書きまちがいとして拾える" $
        unknownChannels ["rakuten", "rakutem"] @?= ["rakutem"]
    , testCase "判定に渡す行は、1販路1行の JSON（id・名前・区分・扱う種類・検索語）" $ do
        let rows = channelRows ["WH-1000XM5"] (filter ((== "rakuten") . (.sid)) catalog)
            field k = case map snd rows of
              [Object o] -> KM.lookup k o
              _ -> Nothing
        map fst rows @?= ["rakuten"]
        (field "id", field "category", field "terms") @?= (Just (toJSON ("rakuten" :: Text)), Just (toJSON ("大手モール" :: Text)), Just (toJSON ["WH-1000XM5" :: Text]))
    , testCase "外のコマンドの出力：販路の id だけの行でも、id を持つ JSON でもよい" $
        commandKept "rakuten\n\"mercari\"\n{\"id\":\"bookoff\"}\n{\"id\":\"hardoff\",\"keep\":false}\n" @?= Right (Set.fromList ["rakuten", "mercari", "bookoff"])
    , testCase "外のコマンドに判定を任せる：当たると答えた販路だけが、検索の一覧に入る" $ do
        r <- channelsIn noJev ["WH-1000XM5"] (scopeOf (Selection [] [Scope.judgeByCommand "grep -E '\"id\":\"(rakuten|mercari)\"'"]))
        let todo = either (const []) (lookups ["WH-1000XM5"] [] . (.chosen)) r
        ordNub (map (\l -> l.channel.sid) todo) @?= ["rakuten", "mercari"]
        fmap (.excluded) r @?= Right [Excluded "--judge-channel-cmd grep -E '\"id\":\"(rakuten|mercari)\"'" (length catalog - 2)]
    , testCase "判定が失敗したら、そこでやめる（全部に当たったことにしない）" $ do
        r <- channelsIn noJev ["x"] (scopeOf (Selection [] [Scope.judgeByCommand "exit 3"]))
        isLeft r @?= True
    ]
 where
  scopeOf sel = Scope sel [] []

-- | sold：語を広げて探す（ヤフオクの検索は、英字と数字の切れ目で切った語ごとに一致を見る）
widenTests :: TestTree
widenTests =
  testGroup
    "語を広げる（sold）"
    [ testCase "英字で終わる型番は、英字を外した語で探し、題にもとの語を含むものを残す" $ do
        Yahuoku.widen "NA-LX127C" @?= Just ("NA-LX127", ["NA-LX127C"])
        Yahuoku.widen "KJ-55X80L" @?= Just ("KJ-55X80", ["KJ-55X80L"])
    , testCase "型番だけを広げ、ほかの語はそのままにする" $
        Yahuoku.widen "パナソニック NA-LX127C" @?= Just ("パナソニック NA-LX127", ["NA-LX127C"])
    , testCase "数字で終わる語と、数字の無い語は広げない" $
        map Yahuoku.widen ["WH-1000XM5", "PS5", "ヘッドホン", "iPhone"] @?= [Nothing, Nothing, Nothing, Nothing]
    , testCase "広げた検索（NA-LX127）の結果から、題に NA-LX127C を含むもの（C・CL・CR）だけを残す" $ do
        html <- readFixture "yahuoku/closedsearch-wide.html"
        let def = fromMaybe (panic "カタログに無い") (lookupSite "yahuoku")
            ex = fromMaybe (panic "一覧のアダプタが無い") (adapterFor def).listX
            ctx = ListCtx "yahuoku" "NA-LX127C" Used "https://auctions.yahoo.co.jp/closedsearch/closedsearch?p=NA-LX127&n=100" 200 stamp
            out = ex ctx (parse html) html
        length out.offers @?= 5
        map (.url) (filter (hasAll ["NA-LX127C"]) out.offers)
          @?= [ "https://paypayfleamarket.yahoo.co.jp/item/z699982448"
              , "https://auctions.yahoo.co.jp/jp/auction/1245406296"
              , "https://auctions.yahoo.co.jp/jp/auction/j1235312236"
              ]
    ]

-- | 何を探すか：見つけた売り物のうち、探している物と認める選び方
acceptTests :: TestTree
acceptTests =
  testGroup
    "何を（Query の accept）"
    [ testCase "条件を全部満たすものを認める。どれがいくつ外したかを数える" $ do
        let r = sift [minPrice 12000, nameHas "wh1000xm5", nameLacks "ケース"] ofSighting cs
        map (\c -> c.offer.url) r.chosen @?= ["u1"]
        r.excluded @?= [Excluded "--min-price 12000" 1, Excluded "--has wh1000xm5" 1, Excluded "--not ケース" 1]
    , testCase "条件の順を変えても、認めるものは同じ（外す数の内訳だけが変わる）" $ do
        let r = sift [nameLacks "ケース", nameHas "wh1000xm5", minPrice 12000] ofSighting cs
        map (\c -> c.offer.url) r.chosen @?= ["u1"]
        map (.count) r.excluded @?= [1, 1, 1]
    , testCase "商品名は、大文字小文字・全角半角・空白・ハイフンの違いを無視して照らす" $
        map (\c -> c.offer.url) (sift [nameHas "WH-1000XM5"] ofSighting cs).chosen @?= ["u1", "u2", "u4"]
    , testCase "価格が分からないものは、価格の条件で外さない（個別ページで分かってから、もう一度かかる）" $ do
        let unknown = Sighting "q" New (emptyOffer SearchList "x" "u9" stamp)
        length (sift [minPrice 12000, maxPrice 50000] ofSighting [unknown]).chosen @?= 1
    , testCase "外のコマンドの出力：id だけの行でも、id を持つ JSON でもよい。keep:false は外す" $
        commandKept "3\n{\"id\":5,\"name\":\"x\"}\n{\"id\":7,\"keep\":false}\n\n" @?= Right (Set.fromList ["3", "5"])
    , testCase "外のコマンドが何も出さなければ、全部外す" $ commandKept "" @?= Right Set.empty
    , testCase "外のコマンドの出力が読めなければ、失敗にする（認めたことにしない）" $
        isLeft (commandKept "これは付属品です\n") @?= True
    , testCase "判定に渡す行は、1件1行の JSON（id・商品名・価格・URL など）" $ do
        let ls = BC.lines (commandInput (map snd (seenRows (map ofSighting cs))))
            field k l = KM.lookup k =<< (decodeStrict' l :: Maybe (KM.KeyMap Value))
        length ls @?= 4
        map (field "id") (take 2 ls) @?= [Just (toJSON (0 :: Int)), Just (toJSON (1 :: Int))]
        (field "name" =<< head ls, field "price" =<< head ls, field "url" =<< head ls) @?= (Just (toJSON ("WH-1000XM5 本体" :: Text)), Just (toJSON (30000 :: Int)), Just (toJSON ("u1" :: Text)))
    , testCase "外のコマンドに判定を任せる：受け取った行をそのまま返すコマンド（cat）は、全部認める" $ do
        r <- select noJev seenRows (Selection [] [judgeByCommand "cat"]) ofSighting cs
        fmap (\x -> (length x.chosen, x.excluded)) r @?= Right (4, [Excluded "--judge-cmd cat" 0])
    , testCase "条件を先に、判定を後にかける（書いた順によらない）" $ do
        r <- select noJev seenRows (Selection [minPrice 12000, nameLacks "本体"] [judgeByCommand "head -n 1"]) ofSighting cs
        fmap (\x -> (map (\c -> c.offer.url) x.chosen, map (.count) x.excluded)) r @?= Right (["u3"], [1, 1, 1])
    , testCase "判定が失敗したら、そこでやめる" $ do
        r <- select noJev seenRows (Selection [] [judgeByCommand "exit 3"]) ofSighting cs
        isLeft r @?= True
    , testCase "jev への頼み：行（id を除く）が状態、質問は はい／いいえ が1つ" $ do
        let row = object ["id" .= (7 :: Int), "name" .= ("WH-1000XM5 ケース" :: Text)]
        jevRequest "typesafe/jev-1.14" "本体か" row
          @?= object
            [ "model" .= ("typesafe/jev-1.14" :: Text)
            , "state" .= object ["name" .= ("WH-1000XM5 ケース" :: Text)]
            , "questions" .= object ["keep" .= object ["type" .= ("noul" :: Text), "instructions" .= ("本体か" :: Text)]]
            ]
    , testCase "jev の答え：「はい」の確率を読む。0.5 以上なら認める" $ do
        let yes = jevAnswer "{\"id\":\"gen-dec-1\",\"answers\":{\"keep\":{\"type\":\"noul\",\"noul\":0.96}},\"usage\":{\"input_tokens\":120}}"
            no = jevAnswer "{\"answers\":{\"keep\":{\"type\":\"noul\",\"noul\":0.04}}}"
        (yes, no) @?= (Right 0.96, Right 0.04)
        (fmap jevKeeps yes, fmap jevKeeps no) @?= (Right True, Right False)
    , testCase "jev が断ったとき・答えが読めないときは、失敗にする（認めたことにも外したことにもしない）" $ do
        isLeft (jevAnswer "{\"answers\":{\"keep\":{\"type\":\"refusal\"}}}") @?= True
        jevAnswer "{\"error\":{\"message\":\"Insufficient credits\",\"code\":402}}" @?= Left "jev の答えを読めなかった：Insufficient credits"
        isLeft (jevAnswer "<html>") @?= True
    , testCase "jev のキーが無ければ、判定は失敗する（黙って全部認めない）" $ do
        r <- select noJev seenRows (Selection [] [judgeByJev "本体か"]) ofSighting cs
        isLeft r @?= True
    ]
 where
  cs =
    [ mkCand "x" "u1" "WH-1000XM5 本体" 30000
    , mkCand "x" "u2" "WH-1000XM5 用 イヤーパッド" 1500
    , mkCand "x" "u3" "WH-1000XM4" 20000
    , mkCand "x" "u4" "ｗｈ－１０００ｘｍ５ ケース" 15000
    ]

-- | search の中身：読むページの選び方と、結果のまとめ（純粋な部分）
searchTests :: TestTree
searchTests =
  testGroup
    "search（何を → どこを → 結果）"
    [ testCase "読むページ：販路と新品・中古の組ごとに、安い順で N 件。同じURLは1つに数える" $ do
        let xs = [mkCand "a" "u1" "n" 300, mkCand "a" "u2" "n" 100, mkCand "a" "u3" "n" 200, mkCand "b" "u4" "n" 50, mkCand "a" "u2" "n" 100]
        map (.url) (cheapest 2 xs) @?= ["u4", "u2", "u3"]
        map (.url) (cheapest 0 xs) @?= []
    , testCase "価格.com の転送URL（アフィリエイトの中継）は、取りに行かない" $
        map tracked ["https://c.kakaku.com/forwarder/forward.aspx?ShopCD=1", "https://kakaku.com/ksearch/jump/?u=x", "https://kakaku.com/item/K0001654211/"] @?= [True, True, False]
    , testCase "まとめ：読めたものが先。読めなかったものは一覧の値のまま残る。個別ページの値で条件に合わなければ外す" $ do
        map (\e -> (e.url, e.fromPage, e.confirmation)) res.findings
          @?= [("https://example.test/1", True, Confirmed), ("https://example.test/3", False, Unconfirmed), ("https://example.test/4", False, Unconfirmed)]
        res.rechecked @?= [Excluded "--min-price 12000" 1]
    , testCase "一覧の値からの変化は、その1件の注記に入る" $
        concatMap (.notes) (filter (.fromPage) res.findings) @?= ["価格 ¥20,500→¥20,000（一覧→個別ページ）"]
    , testCase "名指しのページ（--url）は、選び方で外さない" $ do
        let page = Page "https://example.test/9" Nothing
            r = Search.assemble accept (Selected [] []) (Selected [] []) ([Checked page (mkItem "https://example.test/9" "付属品" 500) []], []) [] []
        map (.url) r.findings @?= ["https://example.test/9"]
    , testCase "表：読んだものに、支払額・税込価格・送料・在庫・販売者・読んだ時刻が並ぶ" $ do
        let t = Report.table 3 15 res
            has part = assertBool (toS ("表に無い：" <> part <> "\n" <> t)) (part `T.isInfixOf` t)
        has "確かめた候補1件：確認済み1・一部0"
        has "確認済み\tx\t¥20,000\t¥20,000\t送料無料\t在庫あり\t-\t店\t11/20 23:00\tWH-1000XM5\thttps://example.test/1\t価格 ¥20,500→¥20,000"
        has "ブラウザで開く：HTTP では読めない販路\tx\t¥21,500"
        has "ブロック：HTTP 403・題「Access Denied」・Server AkamaiGHost（11/20 23:00）\tx\t¥22,000"
        has "何を：一覧で見かけた4件 → --min-price 12000 で0件を外す → 4件。個別ページの値では、--min-price 12000 で1件を外した"
        has ("どこを：カタログの全" <> show (length catalog) <> "販路")
    , testCase "JSON Lines：読んだもの・読めなかったもの・集計の順。どの行にも schema が付く" $ do
        let ls = Report.jsonLines res
            schema l = KM.lookup "schema" =<< (decodeStrict' l :: Maybe (KM.KeyMap Value))
        map schema ls @?= map (Just . toJSON) (["meguri.result/1", "meguri.unread/1", "meguri.unread/1", "meguri.summary/1"] :: [Text])
    , testCase "JSON Lines：ブロックで読めなかったものには、ブロックの事実が付く" $ do
        let blocks = [KM.lookup "block" o | l <- Report.jsonLines res, Just o <- [decodeStrict' l :: Maybe (KM.KeyMap Value)], KM.lookup "schema" o == Just (toJSON ("meguri.unread/1" :: Text))]
        blocks @?= [Just (toJSON (Nothing :: Maybe Block)), Just (toJSON akamai)]
    , testCase "個別ページを読まないとき（--read 0）は、一覧で見かけたものを出す" $ do
        let t = Report.table 0 15 (Search.assemble everything (Selected [] []) (Selected cs []) ([], []) [] cs)
        T.isInfixOf "候補（未確認）4件" t @?= True
        T.isInfixOf "個別ページは読んでいない" t @?= True
    ]
 where
  accept = Selection [minPrice 12000] []
  cs = [mkCand "x" ("https://example.test/" <> show n) "WH-1000XM5" (20000 + n * 500) | n <- [1 .. 4 :: Int]]
  page :: Int -> Page
  page n = Page ("https://example.test/" <> show n) (find (\c -> c.offer.url == "https://example.test/" <> show n) cs)
  checked =
    [ Checked (page 1) (mkItem "https://example.test/1" "WH-1000XM5" 20000) ["価格 ¥20,500→¥20,000（一覧→個別ページ）"]
    , Checked (page 2) (mkItem "https://example.test/2" "WH-1000XM5" 9000) []
    ]
  unread = [Unread (page 3) (OpenInBrowser "HTTP では読めない販路"), Unread (page 4) (SiteBlocked akamai)]
  akamai = Block {url = "https://example.test/4", status = 403, reason = "HTTP 403", title = Just "Access Denied", server = Just "AkamaiGHost", retryAfter = Nothing, at = stamp}
  res = Search.assemble accept (Selected catalog []) (Selected cs [Excluded "--min-price 12000" 0]) (checked, unread) [] cs

-- | 一覧の fixture を、そのサイトのアダプタで読む
listOf :: Text -> FilePath -> Text -> Int -> IO ListOut
listOf siteId file url status = do
  html <- readFixture (toS siteId <> "/" <> file <> ".html")
  let def = fromMaybe (panic "カタログに無い") (lookupSite siteId)
      ex = fromMaybe (panic "一覧のアダプタが無い") (adapterFor def).listX
  pure (ex (ListCtx siteId "WH-1000XM5" New url status stamp) (parse html) html)

-- | 店やモールのページの読み取り
storeTests :: TestTree
storeTests =
  testGroup
    "店・モールのページ"
    [ testCase "JAN はチェックディジットで確かめる" $
        map validEan13 ["4562204636486", "4517161163230", "4562204636487", "456220463648", "45622046364860"] @?= [True, True, False, False, False]
    , testCase "要素に分かれた「61」「円」「（税込）」を読む。手前の文字の数字を巻き込まない" $ do
        taxInclAmount ["ブランド No.5", "61", "円", "（税込）"] @?= Just (Yen 61)
        taxInclAmount ["3M61", "円", "（税込）"] @?= Nothing
        taxInclAmount ["61", "円", "（税抜）"] @?= Nothing
    , testCase "一覧：検索結果の入れ物の外の枠（おすすめ・最近売れた商品）を読まない" $ do
        hands <- listOf "hands" "search" "https://hands.net/search/?q=x" 200
        dinos <- listOf "dinos" "search" "https://www.dinos.co.jp/defaultMall/sitemap/CSfSearchList_001.jsp?KEY_SEARCH=x" 200
        (any (T.isInfixOf "4518731021929" . (.url)) hands.offers, length hands.offers) @?= (False, 3)
        (any (T.isInfixOf "9999999999" . (.url)) dinos.offers, length dinos.offers) @?= (False, 3)
    , testCase "一覧：該当なしのページに商品が並んでいても、0件と読む" $ do
        hands <- listOf "hands" "search-zero" "https://hands.net/search/?q=x" 200
        buyma <- listOf "buyma" "search-zero" "https://www.buyma.com/r/-O3/x/" 404
        ishibashi <- listOf "ishibashi" "search-zero" "https://store.ishibashi.co.jp/search?q=x" 200
        map (\o -> (length o.offers, o.zero)) [hands, buyma, ishibashi] @?= [(0, True), (0, True), (0, True)]
    , testCase "Francfranc：検索結果はJSで描画されるので、一覧は読まない" $
        (isNothing . (.listX) . adapterFor <$> lookupSite "francfranc", (.tier) <$> lookupSite "francfranc") @?= (Just True, Just HttpItem)
    , testCase "ハンズ：値下げ品は、いまの価格の欄（normal）を読む" $ do
        html <- readFixture "hands/item.html"
        let r = extractItem "https://hands.net/goods/4517161163230/" stamp html
        T.isInfixOf "220" html @?= True
        (r.offer.priceIncl.value, r.offer.priceExcl.value, r.offer.keys.jan) @?= (Just (Yen 100), Just (Yen 91), Just "4517161163230")
    , testCase "LOHACO：おすすめの枠の価格と、無効のボタンを読まない" $ do
        html <- readFixture "lohaco/item.html"
        let r = extractItem "https://lohaco.yahoo.co.jp/store/h-lohaco/item/uu42216/" stamp html
        T.isInfixOf "999" html @?= True
        (r.offer.priceIncl.value, r.offer.shipping.value, r.offer.buy.value, payable r.offer)
          @?= (Just (Yen 61), Just (FreeOver (Yen 3780) (Yen 550)), Just (BuyActive "カートに入れる"), Just (Yen 611))
    , testCase "BUYMA：「関税負担なし」があれば表示価格を支払う額として読む。隠してある「売り切れました」は読まない" $ do
        html <- readFixture "buyma/item.html"
        let r = extractItem "https://www.buyma.com/item/120505097/" stamp html
        T.isInfixOf "売り切れました" html @?= True
        (r.offer.priceIncl.value, r.offer.stock.value, r.offer.shipping.value) @?= (Just (Yen 500), Just InStock, Just Free)
    , testCase "BUYMA：「関税負担なし」が無ければ、税込価格は null（額は raw に残す）" $ do
        html <- T.replace "関税負担なし" "" <$> readFixture "buyma/item.html"
        let r = extractItem "https://www.buyma.com/item/120505097/" stamp html
        (r.offer.priceIncl.value, r.offer.priceIncl.raw, payable r.offer) @?= (Nothing, Just "500", Nothing)
        r.confirmation @?= Partial ["税込価格"]
    , testCase "au PAY：税抜の印の商品は、税込価格を null にする" $ do
        html <- T.replace "\"isExclusiveTax\": false" "\"isExclusiveTax\": true" . T.replace "\"isInclusiveTax\": true" "\"isInclusiveTax\": false" <$> readFixture "aupay/item.html"
        let r = extractItem "https://wowma.jp/item/745282614" stamp html
        (r.offer.priceIncl.value, r.offer.priceExcl.value) @?= (Nothing, Just (Yen 560))
    ]

-- | 送料の問い合わせ（配送先は、実行の記録に残さない）
shipTests :: TestTree
shipTests =
  testGroup
    "ship"
    [ testCase "都道府県はコードでも名前でも読む" $
        map (fmap (\p -> (p.code, p.label)) . parsePref) ["13", "01", "東京都", "東京", "47", "0", "48", "江戸"]
          @?= [Just (13, "東京都"), Just (1, "北海道"), Just (13, "東京都"), Just (13, "東京都"), Just (47, "沖縄県"), Nothing, Nothing, Nothing]
    , testCase "ハードオフ：読み取り専用の POST。都道府県名と商品番号を送る" $ do
        let plan = shipPlan tokyo "https://netmall.hardoff.co.jp/product/6220014/"
        fmap (\p -> (p.req.url, p.req.form, p.req.private)) plan
          @?= Just ("https://netmall.hardoff.co.jp/ajax/CHfOmCalcDelvCom_J.jsp", Just [("toPref", "東京都"), ("goodsNo", "6220014")], True)
        (plan >>= \p -> p.readFee "{\"result\":\"SUCCESS\",\"delvCom\":\"600\"}") @?= Just (Yen 600)
        (plan >>= \p -> p.readFee "{\"result\":\"ERROR\"}") @?= Nothing
    , testCase "トレジャーファクトリー：GET。都道府県コードを送る" $ do
        let plan = shipPlan tokyo "https://ec.treasure-f.com/item/2503005450576041"
        fmap (\p -> (p.req.url, p.req.private)) plan
          @?= Just ("https://ec.treasure-f.com/item/postage?code=2503005450576041&prefId=13", True)
        (plan >>= \p -> p.readFee "{ \"postage\": \"1,050\", \"result\": true }") @?= Just (Yen 1050)
    , testCase "送料を計算できないサイトは、問い合わせを作らない" $
        isNothing (shipPlan tokyo "https://item.rakuten.co.jp/cocohouse/nb08qd1ckcd/") @?= True
    ]
 where
  tokyo = Pref 13 "東京都"

-- | rank の枠分け・重複の除き方と、verify の「変化」
rankTests :: TestTree
rankTests =
  testGroup
    "rank・verify"
    [ testCase "オークションは「入札次第」" $
        Rank.classify True (listed "u1" 1000) {stock = got SearchList (Auction 3 "2026-11-22T21:00:00+09:00")} @?= Bidding
    , testCase "「ジャンク」は「訳あり」" $
        Rank.classify True (listed "u1" 1000) {name = got SearchList "【ジャンク品】WH-1000XM5"} @?= Flawed
    , testCase "中古の検索で見つけたものは「中古」" $ Rank.classify True (listed "u1" 1000) @?= Secondhand
    , testCase "状態が新品と読めたら、中古の検索でも「すぐ買える」" $
        Rank.classify True (listed "u1" 1000) {condition = got SearchList "新品"} @?= BuyNow
    , testCase "中古だけを扱うサイトは「中古」" $
        Rank.classify False (listedAt "hardoff" "u1" 1000) @?= Secondhand
    , testCase "売り切れは「いまは買えない」" $
        Rank.classify False (listed "u1" 1000) {stock = got SearchList Sold} @?= Gone
    , testCase "同じURLは、個別ページの記録を残す" $ do
        let es = arrange [confirmedItem "https://example.test/1" 1500 InStock Nothing] [cand "https://example.test/1" 1000 Nothing]
        map (\e -> (e.url, e.confirmation, e.priceIncl)) es @?= [("https://example.test/1", Confirmed, Just (Yen 1500))]
    , testCase "一覧の候補は、安くても確認済みより後ろ。判定は未確認のまま" $ do
        let es = arrange [confirmedItem "https://example.test/1" 1500 InStock Nothing] [cand "https://example.test/2" 1000 Nothing]
        map (\e -> (e.url, e.confirmation)) es @?= [("https://example.test/1", Confirmed), ("https://example.test/2", Unconfirmed)]
    , testCase "同じ在庫は1つにして、確認済みの方を残す" $ do
        let es = arrange [confirmedItem "https://example.test/1" 1500 InStock (Just "z1")] [cand "https://example.test/2" 1000 (Just "z1")]
        map (\e -> (e.url, e.duplicates)) es @?= [("https://example.test/1", ["https://example.test/2"])]
    , testCase "一覧と個別ページで価格が違えば、変化として出す" $
        driftFrom (cand "https://example.test/1" 1000 Nothing) (confirmedItem "https://example.test/1" 1500 InStock Nothing)
          @?= ["価格 ¥1,000→¥1,500（一覧→個別ページ）"]
    , testCase "一覧と個別ページで同じなら、変化なし" $
        driftFrom (cand "https://example.test/1" 1500 Nothing) (confirmedItem "https://example.test/1" 1500 InStock Nothing) @?= []
    ]
 where
  listed :: Text -> Int -> Offer OnList
  listed = listedAt "x"
  listedAt :: Text -> Text -> Int -> Offer OnList
  listedAt s u p = (emptyOffer SearchList s u stamp) {name = got SearchList "WH-1000XM5", priceIncl = got SearchList (Yen p), shipping = got SearchList Free}
  cand u p shared = Sighting "q" New ((listed u p) {keys = noKeys {sharedStockId = shared}})
  confirmedItem :: Text -> Int -> Stock -> Maybe Text -> Reading
  confirmedItem u p st shared =
    toReading
      True
      (emptyOffer Body "x" u stamp)
        { name = got JsonLd "WH-1000XM5"
        , priceIncl = got JsonLd (Yen p)
        , shipping = got Body Free
        , stock = got JsonLd st
        , seller = got Body "店"
        , image = got OpenGraph "https://example.test/1.jpg"
        , buy = got Body (BuyActive "カートに入れる")
        , keys = noKeys {sharedStockId = shared}
        }

fetchTests :: TestTree
fetchTests =
  testGroup
    "応答の分類"
    [ testCase "403 はブロック" $ blockSign 403 "" @?= Just "HTTP 403"
    , testCase "429 はブロック" $ blockSign 429 "" @?= Just "HTTP 429"
    , testCase "200 でも、ページの頭に Access Denied があればブロック" $ blockSign 200 "<html><title>Access Denied</title>" @?= Just "ページに「Access Denied」"
    , testCase "状態コードと目印の両方があれば、両方を根拠にする" $ blockSign 403 "<title>Just a moment...</title>" @?= Just "HTTP 403・ページに「Just a moment...」"
    , testCase "ふつうのページはブロックではない" $ blockSign 200 "<html>ok" @?= Nothing
    , testCase "ブロックの事実：ページ・状態コード・根拠・ページの題・Server・Retry-After・取った時刻" $
        blockOf (deniedPage "https://example.test/a")
          @?= Just Block {url = "https://example.test/a", status = 403, reason = "HTTP 403・ページに「Access Denied」", title = Just "Access Denied", server = Just "AkamaiGHost", retryAfter = Just "120", at = stamp}
    , testCase "ブロックでない応答には、ブロックの事実が無い" $
        blockOf (deniedPage "https://example.test/a") {status = 200, body = "<html><title>商品</title>"} @?= Nothing
    , testCase "メンテナンスは停止" $ classify 503 "ただいまメンテナンス中です" @?= PageStopped "メンテナンスのページ"
    , testCase "200 でも、短い「メンテナンスのお知らせ」のページは停止" $
        classify 200 "<title>メンテナンスのお知らせ</title>DCMオンライン 一時停止のお知らせ" @?= PageStopped "メンテナンスのページ"
    , testCase "404 は「無い」" $ classify 404 "<html>not found" @?= PageNotFound
    , testCase "200 は成功" $ classify 200 "<html>ok" @?= PageOk
    , testCase "500 は失敗" $ classify 500 "<html>error" @?= PageError "HTTP 500"
    ]

-- | テスト用の、ブロックの応答（Akamai の Access Denied）。取った時刻は 'stamp'
deniedPage :: Text -> Fetched
deniedPage u =
  Fetched
    { url = u
    , status = 403
    , contentType = Just "text/html"
    , headers = [("Server", "AkamaiGHost"), ("Retry-After", "120")]
    , body = "<html><head><title>Access Denied</title></head><body>You don't have permission</body></html>"
    , fetchedAt = UTCTime (fromGregorian 2026 11 20) (14 * 3600)
    }

-- | meguri を同時にいくつ走らせてもよいこと：実行の記録のフォルダ、掃除、同じホストへの順番
concurrencyTests :: TestTree
concurrencyTests =
  testGroup
    "同時に走らせる"
    [ testCase "同じ秒に走らせても、実行の記録は別のフォルダに書く" $ withTempDir $ \d -> do
        runs <- mapConcurrently (const (newRun (paths d))) [1 .. 5 :: Int]
        length (ordNub (map snd runs)) @?= 5
        for_ runs $ \(_, dir) -> doesDirectoryExist dir >>= (@?= True)
    , testCase "掃除を2つ同時に走らせても落ちず、7日より古い記録と、古い版の残りだけを消す" $ withTempDir $ \d -> do
        let p = paths d
        old <- addUTCTime (-8 * 86400) <$> getCurrentTime
        createDirectoryIfMissing True p.runsDir
        for_ [1 .. 300 :: Int] $ \i -> do
          let dir = p.runsDir </> ("old-" <> show i)
          createDirectory dir
          setModificationTime dir old
        createDirectory (p.runsDir </> "new")
        writeFile (p.runsDir </> "latest") "20261121-115633"
        createDirectoryIfMissing True (d </> "http")
        writeFile (d </> "http" </> "x.meta") "{}"
        _ <- concurrently (purgeOld p) (purgeOld p)
        listDirectory p.runsDir >>= (@?= ["new"])
        doesDirectoryExist (d </> "http") >>= (@?= False)
    , testCase "同じホストへは、ロックファイルを分け合うほかの取得器（ほかのプロセス）とあわせても2秒空ける" $ withTempDir $ \d -> do
        f1 <- newFetcher (d </> "locks") Nothing
        f2 <- newFetcher (d </> "locks") Nothing
        (t1, t2) <- concurrently (takeTurn f1 "example.com" getCurrentTime) (takeTurn f2 "example.com" getCurrentTime)
        assertBool "2秒空いていない" (abs (diffUTCTime t2 t1) >= 1.9)
    , testCase "ホストが違えば待たない" $ withTempDir $ \d -> do
        f <- newFetcher (d </> "locks") Nothing
        (t1, t2) <- concurrently (takeTurn f "a.example.com" getCurrentTime) (takeTurn f "b.example.com" getCurrentTime)
        assertBool "待っている" (abs (diffUTCTime t2 t1) < 1)
    ]
 where
  paths d = Paths {runsDir = d </> "runs", locksDir = d </> "locks", leftovers = [d </> "http", d </> "judge", d </> "runs" </> "latest"]}

-- | 使い捨てのフォルダの中で試す
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "meguri-test"

-- | microdata は Product の範囲の中だけを読む
microdataTests :: TestTree
microdataTests =
  testGroup
    "microdata"
    [ testCase "パンくずリストの名前を商品名にしない" $
        itemprop "name" (parse breadcrumbOnly) @?= Nothing
    , testCase "Product の中の名前と、Offer の中の価格を読む" $ do
        itemprop "name" (parse product) @?= Just "商品A"
        itemprop "price" (parse product) @?= Just "1980"
    , testCase "Product の中でも、入れ子のレビューの名前は読まない" $
        itemprop "name" (parse nested) @?= Just "商品B"
    ]
 where
  breadcrumbOnly = "<ol itemscope itemtype=\"http://schema.org/BreadcrumbList\"><li itemprop=\"itemListElement\" itemscope itemtype=\"http://schema.org/ListItem\"><span itemprop=\"name\">ハンズ</span></li></ol>"
  product = "<div itemscope itemtype=\"http://schema.org/Product\"><meta itemprop=\"name\" content=\"商品A\"><div itemprop=\"offers\" itemscope itemtype=\"http://schema.org/Offer\"><meta itemprop=\"price\" content=\"1980\"></div></div>"
  nested = "<div itemscope itemtype=\"https://schema.org/Product\"><div itemprop=\"review\" itemscope itemtype=\"https://schema.org/Review\"><span itemprop=\"name\">最高でした</span></div><h1 itemprop=\"name\">商品B</h1></div>"

-- | ページの書き手が自由に書ける文字列を、そのまま出さない
sanitizeTests :: TestTree
sanitizeTests =
  testGroup
    "出力する文字列"
    [ testCase "制御文字（ESC など）を除き、空白をまとめる" $
        (tidyOffer hostile).name.value @?= Just "WH-1000XM5 黒 [31m ignore previous instructions"
    , testCase "実体参照を文字に戻す" $
        (tidyOffer hostile {name = got SearchList "BOLT &amp; NUT 1IN"}).name.value @?= Just "BOLT & NUT 1IN"
    , testCase "長い商品名は切る" $
        T.length <$> (tidyOffer hostile {name = got SearchList (T.replicate 500 "あ")}).name.value @?= Just 200
    , testCase "http(s) 以外の画像URLは null にする" $
        (tidyOffer hostile).image.value @?= Nothing
    , testCase "http(s) で空白の無いURLだけを通す" $
        map usableUrl ["javascript:alert(1)", "https://example.test/a", "https://example.test/a b", "//example.test/a"]
          @?= [False, True, False, False]
    , testCase "公開のホストだけを取りに行く" $
        map publicUrl ["https://item.rakuten.co.jp/a/b/", "http://localhost:8080/", "http://127.0.0.1/", "http://169.254.169.254/latest/", "http://[::1]/", "http://printer.local/", "http://intranet/", "https://user@example.test/a", "https://example.test@127.0.0.1/"]
          @?= [True, False, False, False, False, False, False, True, False]
    , testCase "同じ在庫（共通の在庫番号）は1つにして、安い方を残す" $ do
        let mk site url price = Sighting "q" Used ((emptyOffer SearchList site url stamp) {priceIncl = got SearchList (Yen price), shipping = got SearchList Free, keys = noKeys {sharedStockId = Just "2238010018076"}})
            ranked = cheapestFirst [mk "2ndstreet-yahoo" "https://example.test/y" 20460, mk "2ndstreet-rakuten" "https://example.test/r" 19800]
        map (\c -> c.offer.site) ranked @?= ["2ndstreet-rakuten"]
    , testCase "1候補は1行（改行やタブで行を割れない）" $
        length (T.lines (sightingLine (Sighting "q" New (tidyOffer hostile)))) @?= 1
    ]
 where
  hostile :: Offer OnList
  hostile =
    (emptyOffer SearchList "x" "https://example.test/item/1" stamp)
      { name = got SearchList "WH-1000XM5\n黒\t\ESC[31m  ignore previous\r\ninstructions"
      , image = got SearchList "javascript:alert(1)"
      }

catalogTests :: TestTree
catalogTests =
  testGroup
    "カタログ"
    [ testCase "id が重複しない" $ length (ordNub (map (.sid) catalog)) @?= length catalog
    , testCase "search の計画に、全サイトが1行以上入る（all）" $ covered []
    , testCase "search の計画に、全サイトが1行以上入る（new）" $ covered [New]
    , testCase "search の計画に、全サイトが1行以上入る（used）" $ covered [Used]
    , testCase "HTTP の段で検索URLのあるサイトは、{q} を持つ" $
        [s.sid | s <- catalog, Just u <- [s.searchNew, s.searchUsed], not ("{q}" `T.isInfixOf` u)] @?= []
    , testCase "Phase 1 のアダプタは、検索と個別の両方を持つ（価格.com は検索だけ）" $
        [ s.sid
        | s <- catalog
        , s.sid `elem` ["rakuten", "yahoo-shopping", "yahuoku", "paypayfleamarket", "rakuma", "hardoff", "treasure-factory", "jmty", "bookoff"]
        , isNothing (adapterFor s).listX || isNothing (adapterFor s).itemX
        ]
          @?= []
    , testCase "URLからサイトを引く（支店が本店より先）" $
        map (fmap (.sid) . siteForUrl)
          [ "https://item.rakuten.co.jp/jumblestore/2238010018076/"
          , "https://item.rakuten.co.jp/cocohouse/nb08qd1ckcd/"
          , "https://store.shopping.yahoo.co.jp/ssol-shopping/2327288472427.html"
          , "https://netmall.hardoff.co.jp/product/6220014/"
          , "https://example.test/"
          ]
          @?= [Just "2ndstreet-rakuten", Just "rakuten", Just "2ndstreet-yahoo", Just "hardoff", Nothing]
    , testCase "channels.md に全サイトが載る" $
        [s.sid | s <- catalog, not (("`" <> s.sid <> "`") `T.isInfixOf` sitesMd [])] @?= []
    , testCase "集計は、サイトごとに1行" $ do
        let cov = [Coverage "rakuten" q k (Found 3 (Just (Yen 100))) Nothing "HTTP" stamp | q <- ["a", "b"], k <- [New, Used]]
        length (T.lines (coverageSummary cov)) @?= 2
    , testCase "集計：ブロックの行には、根拠・題・Server・時刻と、ブロックされたページのURLが出る。同じブロックは1つにまとめる" $ do
        let b = Block {url = "https://search.rakuten.co.jp/search/mall/a/", status = 403, reason = "HTTP 403", title = Just "Access Denied", server = Just "AkamaiGHost", retryAfter = Nothing, at = stamp}
            cov = [Coverage "rakuten" q New (Blocked b) (Just ("https://search.rakuten.co.jp/search/mall/" <> q <> "/")) "HTTP" stamp | q <- ["a", "b"]]
        T.lines (coverageSummary cov)
          @?= [ "どこまで探したか：ブロック2（サイト×検索語×種類の行数）"
              , "rakuten\t「a」新 ブロック：HTTP 403・題「Access Denied」・Server AkamaiGHost（11/20 23:00）\thttps://search.rakuten.co.jp/search/mall/a/"
              ]
    ]
 where
  covered sel =
    let jobs = lookups ["WH-1000XM5"] sel catalog
     in [s.sid | s <- catalog, s.sid `notElem` map (\j -> j.channel.sid) jobs] @?= []

_unusedValue :: Value
_unusedValue = toJSON ()

-- | check の純粋な部分：個別ページのリンク拾いと、結果の書き出し
checkTests :: TestTree
checkTests =
  testGroup
    "check"
    [ testCase "検索結果から、個別ページの形に合うリンクだけを拾う（相対URL・実体参照・重複）" $ do
        let html = "<a href=\"/store/g/g4902011649215/\">a</a><a href=\"/store/g/g4902011649215/\">b</a><a href=\"/store/guide/\">c</a><a href=\"https://www.loft.co.jp/store/g/g1/?a=1&amp;b=2\">d</a><a href=\"javascript:void(0)\">e</a>"
            loft = fromMaybe (panic "カタログに無い") (lookupSite "loft")
        itemLinks loft "https://www.loft.co.jp/store/goods/search.aspx?keyword=x" (parse html)
          @?= ["https://www.loft.co.jp/store/g/g4902011649215/", "https://www.loft.co.jp/store/g/g1/?a=1&b=2"]
    , testCase "診断の結果は、日本語のまま Haskell の文字列として書き出す" $ do
        let src = moduleSource [Health "rakuten" "2026-11-21" "検索：取得45件" "個別：\"確認済み\" \\" True]
        T.isInfixOf "  [ (\"rakuten\", \"2026-11-21\", \"検索：取得45件\", \"個別：\\\"確認済み\\\" \\\\\", True)" src @?= True
        T.isInfixOf "module Meguri.Catalog.Health (rows) where" src @?= True
    , testCase "書き出した記録は、そのまま読み戻せる" $ do
        let hs = [Health "rakuten" "2026-11-21" "検索：取得45件" "個別：\"確認済み\" \\" True, Health "clas" "2026-11-21" "検索：HTTP 200（商品リンクなし）" "個別：見本なし" False]
        parseModule (moduleSource hs) @?= hs
    , testCase "結果が空でも、読めるモジュールになる" $
        T.isInfixOf "rows =\n  []" (moduleSource []) @?= True
    , testCase "一部のサイトだけ診断したら、ほかのサイトの記録は残す" $ do
        let old = [Health "a" "2026-11-20" "s" "i" True, Health "b" "2026-11-20" "s" "i" True]
            new = [Health "b" "2026-11-21" "s2" "i2" False]
        map (\h -> (h.sid, h.day)) (merge new old) @?= [("b", "2026-11-21"), ("a", "2026-11-20")]
    ]
