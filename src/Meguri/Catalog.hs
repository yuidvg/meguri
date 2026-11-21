-- | 販路の一覧。サイトの知識の出どころは、このファイル1つだけ。
--
-- docs/channels.md は @meguri sites --format md@ で、ここから作る。
-- 注記（hints）は、確かめた日（checked）に実際に開いて分かったことで、決まった手順ではない。
module Meguri.Catalog
  ( Channel (..)
  , Category (..)
  , SiteKind (..)
  , Tier (..)
  , Enc (..)
  , catalog
  , lookupSite
  , categoryName
  , tierLabel
  , kindLabel
  , excludedNote
  ) where

import Data.List qualified as L
import Data.Time (Day, fromGregorian)
import Protolude

data Category
  = PriceComparison
  | Mall
  | DirectAndSpecialty
  | Electronics
  | Interior
  | HomeCenter
  | FleaAuction
  | Reuse
  | OverseasRental
  | NonPrice
  | ItemSpecific
  deriving stock (Eq, Ord, Show, Enum, Bounded)

categoryName :: Category -> Text
categoryName = \case
  PriceComparison -> "価格比較（候補探し用）"
  Mall -> "大手モール"
  DirectAndSpecialty -> "通販・メーカー公式・専門店"
  Electronics -> "家電量販"
  Interior -> "インテリア・雑貨"
  HomeCenter -> "ホームセンター・法人通販"
  FleaAuction -> "フリマ・オークション・個人間"
  Reuse -> "リユース店"
  OverseasRental -> "海外・レンタル"
  NonPrice -> "価格以外の情報源"
  ItemSpecific -> "品目別の専門店"

-- | 扱う種類
data SiteKind = KNew | KUsed | KAuction | KP2P | KRental | KInfo
  deriving stock (Eq, Show)

kindLabel :: SiteKind -> Text
kindLabel = \case
  KNew -> "新品"
  KUsed -> "中古"
  KAuction -> "オークション"
  KP2P -> "個人間"
  KRental -> "レンタル"
  KInfo -> "情報"

-- | どの方法で読めるか
data Tier
  = -- | 検索も個別ページも HTTP で取れる
    Http
  | -- | 個別ページだけ HTTP で取れる（検索はブラウザ）
    HttpItem
  | -- | JS の描画が要る（ヘッドレスでよい）
    BrowserJs
  | -- | 画面ありの通常のブラウザが要る
    BrowserNormal
  | -- | 確かめた日に止まっていた（注記つき）
    TierStopped Text
  | -- | どの方法でも読めなかった
    TierBlocked
  | -- | 買う販路ではない
    TierOutOfScope
  deriving stock (Eq, Show)

tierLabel :: Tier -> Text
tierLabel = \case
  Http -> "HTTP可"
  HttpItem -> "個別ページだけHTTP可"
  BrowserJs -> "ブラウザ要"
  BrowserNormal -> "通常のブラウザ要"
  TierStopped _ -> "停止中"
  TierBlocked -> "不可"
  TierOutOfScope -> "対象外"

-- | 検索語の文字コード
data Enc = Utf8 | ShiftJis | Cp932 | EucJp
  deriving stock (Eq, Show)

data Channel = Channel
  { sid :: Text
  , label :: Text
  , category :: Category
  , kinds :: [SiteKind]
  , tier :: Tier
  , -- | 抽出に使うアダプタの id（支店は本店のアダプタを使う）
    adapter :: Text
  , enc :: Enc
  , -- | 新品（または区別なし）の検索URL。@{q}@ に検索語が入る
    searchNew :: Maybe Text
  , -- | 中古の検索URL
    searchUsed :: Maybe Text
  , -- | 検索URLを組み立てられないときの入り方
    searchNote :: Maybe Text
  , -- | 個別ページのURLの形（正規表現）。URLからサイトを引くのに使う
    itemPatterns :: [Text]
  , hints :: [Text]
  , -- | 注記を、実際に開いて確かめた日
    checked :: Day
  , -- | 健康診断（meguri check）で使う検索語。そのサイトで必ず何か出る語にする
    sample :: Text
  , -- | 健康診断で読む個別ページ。検索から個別ページをたどれないサイト用（長く残るページにする）
    sampleItem :: Maybe Text
  , -- | 止まっているサイトの入口。再開したかを健康診断で見る
    home :: Maybe Text
  }

d1120 :: Day
d1120 = fromGregorian 2026 11 20

site :: Text -> Text -> Category -> [SiteKind] -> Tier -> Channel
site sid label category kinds tier =
  Channel
    { sid
    , label
    , category
    , kinds
    , tier
    , adapter = sid
    , enc = Utf8
    , searchNew = Nothing
    , searchUsed = Nothing
    , searchNote = Nothing
    , itemPatterns = []
    , hints = []
    , checked = d1120
    , sample = "WH-1000XM5"
    , sampleItem = Nothing
    , home = Nothing
    }

lookupSite :: Text -> Maybe Channel
lookupSite i = find ((== i) . (.sid)) catalog

-- | channels.md の「一覧から外したもの」
excludedNote :: Text
excludedNote = "インテリアズー、ルカリカ、エコリン村、ドンドンダウン：いまの通販のURLを特定できなかった（ルカリカのドメインは売りに出ている）。"

catalog :: [Channel]
catalog = map withSamples entries
 where
  withSamples s =
    s
      { sample = fromMaybe s.sample (L.lookup s.sid sampleQueries)
      , sampleItem = L.lookup s.sid sampleItems
      , home = L.lookup s.sid homes
      }

-- | 健康診断の検索語（ここに無いサイトは WH-1000XM5）
sampleQueries :: [(Text, Text)]
sampleQueries =
  concat
    [ map (,"ティッシュ") ["lohaco", "costco"]
    , map (,"掃除機") ["japanet", "shopch", "qvc", "bellemaison", "edion-rakuten", "joshin-rakuten"]
    , map (,"ソファ") ["dinos"]
    , map (,"加湿器") ["cado"]
    , map (,"時計") ["plywood"]
    , map (,"椅子") ["ikea", "actus", "unico", "idee", "clas", "nitori"]
    , map (,"タオル") ["loft", "hands", "francfranc"]
    , map (,"脚立") ["cainz", "kohnan", "vivahome", "monotaro"]
    , map (,"コピー用紙") ["tanomail", "kaunet", "askul"]
    , map (,"ヘッドホン") ["mbok", "jmty", "treasure-factory", "okoku", "wonderrex", "aucfan"]
    , map (,"ハリーポッター") ["bookoff", "rakuten-books", "honto"]
    , map (,"財布") ["buyma", "komehyo"]
    , map (,"iPhone 13") ["iosys", "janpara"]
    , map (,"ギター") ["ishibashi", "digimart"]
    , map (,"ベビーカー") ["akachan", "babiesrus"]
    , map (,"スニーカー") ["locondo", "zozotown"]
    ]

-- | 健康診断で読む個別ページ（検索からたどれないサイト）
sampleItems :: [(Text, Text)]
sampleItems =
  [ ("balmuda", "https://store.balmuda.com/disp/CBlSfSelectGoodsPage.jsp?PRODUCT_SERIES=K11A")
  , ("kinokuniya", "https://www.kinokuniya.co.jp/f/dsg-01-9784863891609")
  ]

-- | 止まっているサイトの入口
homes :: [(Text, Text)]
homes =
  [ ("irisplaza", "https://www.irisplaza.co.jp/")
  , ("edion", "https://www.edion.com/")
  , ("donki", "https://shop.donki.com/")
  , ("muji", "https://www.muji.com/jp/ja/store")
  , ("dcm", "https://www.dcm-ekurashi.com/")
  ]

entries :: [Channel]
entries =
  -- 価格比較 ---------------------------------------------------------------
  [ (site "kakaku" "価格.com" PriceComparison [KNew] Http)
      { enc = Cp932
      , searchNew = Just "https://kakaku.com/search_results/{q}/"
      , itemPatterns = ["^https://kakaku\\.com/item/[KJ][0-9]+/"]
      , hints =
          [ "文字コードが cp932。検索語も cp932 でエンコードし、HTMLも cp932 で読む。"
          , "/item/{K}/shop/ のショップ一覧で、各店の価格・送料・在庫・ポイントを拾える。「売り場へ行く」ボタンの onclick にある k3c.atrack.ping({shpname, shpbid, shpshp, shppnt}) が確実。同じ店がタブ違いで重複して出るので除く。"
          , "大きい画像は img1.kakaku.k-img.com/images/productimage/fullscale/{K}.jpg（kakaku.com/images/… は404）。"
          , "ここで見た値は、その店のページを見たことにならない（未確認として扱う）。"
          , "2026-11-21：JAN で探すと、製品ページではなく、モールの出品の一覧（ショッピング検索）が出る。meguri は製品ページだけを読むので、0件になる。型番か商品名で探す。"
          ]
      }
  , (site "google-shopping" "Googleショッピング" PriceComparison [KNew] BrowserNormal)
      { searchNew = Just "https://www.google.com/search?udm=28&q={q}"
      , hints = ["ヘッドレスではボット確認（/sorry/）が出たが、通常のブラウザでは一覧と価格が見られた。"]
      }
  , (site "keepa" "Keepa" PriceComparison [KInfo] TierBlocked)
      { hints =
          [ "ページ本体は、通常のブラウザでも「Anti-bot check」で止まった。"
          , "ASIN が分かれば、graph.keepa.com/pricehistory.png?asin={ASIN}&domain=co.jp&amazon=1&new=1&used=1 を取ると、新品・中古の価格の推移を画像で見られる。"
          ]
      }
  , -- 大手モール -------------------------------------------------------------
    (site "amazon" "Amazon.co.jp" Mall [KNew, KUsed] BrowserJs)
      { searchNew = Just "https://www.amazon.co.jp/s?k={q}&s=price-asc-rank"
      , itemPatterns = ["^https://www\\.amazon\\.co\\.jp/(.*/)?dp/[A-Z0-9]{10}"]
      , hints =
          [ "検索は HTTP だと503になるので、ブラウザで開く。"
          , "個別 /dp/{ASIN}：価格は #corePriceDisplay、在庫は #availability、販売者は #merchantInfoFeature_feature_div、送料は配送ブロック、大きい画像は #landingImage の data-old-hires。"
          , "出品者別・中古は /dp/{ASIN}?aod=1 で見る（/gp/aod/ajax を直接開くと見つからない）。"
          , "安い順の先頭は付属品のことが多い。状態の絞り込み（整備済み品・中古品）は、検索結果の左のリンクから取る。"
          , "公式APIは条件が厳しいので使わない。"
          , "抽出では、支払う価格（priceToPay）を読む。隣の参考価格（定価）は読まない。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で /dp/{ASIN} を開けて、HTMLを保存できた。お届け先の地名と郵便番号が入るので、保存したHTMLを人に渡さない。"
          ]
      }
  , (site "amazon-renewed" "Amazon整備済み品" Mall [KUsed] BrowserJs)
      { adapter = "amazon"
      , searchUsed = Just "https://www.amazon.co.jp/s?k={q}%20%E6%95%B4%E5%82%99%E6%B8%88%E3%81%BF%E5%93%81&s=price-asc-rank"
      , hints = ["検索語に「整備済み品」を足す。個別ページに「整備済み品 - 非常に良い」などの状態と、保証の文言が出る。"]
      }
  , (site "rakuten" "楽天市場" Mall [KNew, KUsed] Http)
      { searchNew = Just "https://search.rakuten.co.jp/search/mall/{q}/?s=2"
      , searchUsed = Just "https://search.rakuten.co.jp/search/mall/{q}/?s=2&used=1"
      , itemPatterns = ["^https://item\\.rakuten\\.co\\.jp/[^/]+/[^/]+"]
      , hints =
          [ "検索結果は、data-link=\"item\" の要素から商品URLが取れる。店内だけ探すなら &sid={店ID}。"
          , "個別ページは EUC-JP。価格と在庫は microdata の itemprop=\"price|availability\"、店名は埋め込みJSONの shopName にある（JSON-LD に offers は無い）。"
          , "送料は、埋め込みJSONの postageIncluded が true なら無料。false なら配送先しだいで、HTMLからは額が読めない。"
          , "続けて検索すると503が出るので、間隔を空ける。"
          , "URLがASINの形（/b0xxxxxxxx/）の店は、無在庫転売。"
          ]
      }
  , (site "yahoo-shopping" "Yahoo!ショッピング" Mall [KNew, KUsed] Http)
      { searchNew = Just "https://shopping.yahoo.co.jp/search?p={q}&X=2"
      , searchUsed = Just "https://shopping.yahoo.co.jp/search?p={q}&X=2&used=1"
      , itemPatterns = ["^https://store\\.shopping\\.yahoo\\.co\\.jp/[^/]+/[^/]+\\.html"]
      , hints =
          [ "検索結果は、HTMLの data-beacon（_cl_link:title の方）に prc・point・targurl・tname・jan が入っている。shipment の値は送料無料かどうかと合わなかったので使わない。"
          , "個別ページは、__NEXT_DATA__ の pageProps に item（価格・JAN・在庫数）・postage（送料）・seller（店名）・cartButton がある。JSON-LD の Product にも価格・在庫がある。"
          , "送料は配送先で、ポイントはログインで変わる。"
          , "中古では、2nd STREET の支店がまとまって出る。"
          ]
      }
  , (site "lohaco" "LOHACO" Mall [KNew] HttpItem)
      { searchNew = Just "https://lohaco.yahoo.co.jp/search?p={q}&sort=%2Bprice"
      , itemPatterns = ["^https://lohaco\\.yahoo\\.co\\.jp/store/[^/]+/item/"]
      , hints =
          [ "検索結果はJSで描画されるので、ブラウザで開く（?q= はトップに戻される）。"
          , "個別ページは HTTP でも、価格・在庫・送料条件が読める。"
          , "2026-11-21：検索は、ヘッドレスの agent-browser で開いて商品リンクを拾えた。"
          , "2026-11-21：抽出では、h1 から「こちらの商品もおすすめ」の見出しまでの本文を読む（クラス名はビルドごとに変わる）。価格は「61」「円」「（税込）」と要素が分かれて並ぶ。販売者は「この商品は◯◯が販売・発送します」、送料は「3,780円(税込)以上で基本配送料550円(税込)無料」の形。"
          ]
      }
  , (site "aupay" "au PAYマーケット" Mall [KNew, KUsed] HttpItem)
      { searchNew = Just "https://wowma.jp/itemlist?keyword={q}&sort1=price,A"
      , itemPatterns = ["^https://wowma\\.jp/item/[0-9]+"]
      , hints =
          [ "検索結果はJSで描画されるので、ブラウザで数秒待って読む。"
          , "個別 /item/{番号} は HTTP でも、__NEXT_DATA__ に価格・店名・在庫数・画像がある。"
          , "中古は、商品名の「(中古品)」で見分ける。"
          , "2026-11-21：検索は、ヘッドレスの agent-browser で開いて商品リンクを拾えた。"
          , "2026-11-21：抽出では、__NEXT_DATA__ の initialState.itemDetails.itemPageInfo（currentPrice・taxTypes・remainingItemCount・shopSummaryInfo.shopName）を読む。送料とカートのボタンはJSで描画されるので、HTTP では読めない。"
          ]
      }
  , (site "qoo10" "Qoo10" Mall [KNew] BrowserJs)
      { searchNew = Just "https://www.qoo10.jp/gmkt.inc/Search/Default.aspx?keyword={q}"
      , itemPatterns = ["^https://www\\.qoo10\\.jp/g/[0-9]+"]
      , hints =
          [ "HTTP は523エラーになるので、ブラウザで開く。/s/{語} はランキングに飛ぶ。"
          , "個別 /g/{番号} の JSON-LD に、価格（セール時の価格が入ることがある）と在庫がある。安い順はURLでは効かないので、画面で選ぶ。"
          ]
      }
  , (site "dshopping" "dショッピング" Mall [KNew] BrowserNormal)
      { searchNew = Just "https://dshopping.docomo.ne.jp/products_search?keyword={q}&sort=sales_price_asc"
      , itemPatterns = ["^https://dshopping\\.docomo\\.ne\\.jp/products/"]
      , hints =
          [ "HTTP・ヘッドレスは Access Denied だが、通常のブラウザなら数秒待つと開く。"
          , "個別 /products/{番号}?sku=… は、JSON-LD に価格・在庫、本文に販売店・送料・dポイント、大きい画像がある。"
          ]
      }
  , -- 通販・メーカー公式・専門店 -----------------------------------------------
    (site "bellemaison" "ベルメゾン" DirectAndSpecialty [KNew] BrowserNormal)
      { searchNote = Just "検索窓から入る（/shop/app/catalog/list/?keyword={語}）"
      , itemPatterns = ["^https://www\\.bellemaison\\.jp/shop/commodity/"]
      , hints =
          [ "HTTP・ヘッドレスは Akamai に拒否されるが、通常のブラウザなら開く。"
          , "個別 /shop/commodity/0000/{番号} で、税込価格・ポイント・送料条件・カートが読める（JSON-LD は無い）。"
          ]
      }
  , (site "dinos" "ディノス" DirectAndSpecialty [KNew] Http)
      { enc = ShiftJis
      , searchNew = Just "https://www.dinos.co.jp/defaultMall/sitemap/CSfSearchList_001.jsp?KEY_SEARCH={q}&sort=1"
      , itemPatterns = ["^https://www\\.dinos\\.co\\.jp/p/[0-9]+"]
      , hints =
          [ "ページも検索語も Shift_JIS。"
          , "個別ページは、JSON-LD の offers.priceSpecification.price が税込価格。在庫も priceSpecification の中にある。送料は本文の「基本配送料」。"
          , "本文の「在庫なし」は隠してあるひな形（display:none）で、在庫ありの商品にも入っている。読まない。"
          , "2026-11-21：検索結果は #listPreview の中だけ。その後ろに、検索と関係のない商品の枠が並ぶ。型番で探すと、関係の薄い商品が数件返る（サイトの検索の癖）。"
          ]
      }
  , (site "japanet" "ジャパネットたかた" DirectAndSpecialty [KNew] Http)
      { searchNew = Just "https://www.japanet.co.jp/shopping/swd/{q}/"
      , itemPatterns = ["^https://www\\.japanet\\.co\\.jp/shopping/keyword/catdetail/"]
      , hints =
          [ "個別ページは、JSON-LD の offers.Price（大文字の P）が価格で、大きい画像は -l.jpg。送料は本文の「お支払総額（送料○円）」。ページは cp932。"
          , "検索結果の商品リンクは /shopping/keyword/catdetail/…。一覧には「下取りなし価格」と「下取り後価格」が並ぶ。売値は product-price-cell の欄。"
          ]
      }
  , (site "qvc" "QVCジャパン" DirectAndSpecialty [KNew] BrowserNormal)
      { searchNew = Just "https://qvc.jp/catalog/search.html?keyword={q}"
      , itemPatterns = ["^https://qvc\\.jp/product\\."]
      , hints =
          [ "HTTP・ヘッドレスは Akamai の通信エラー（HTTP は418）だが、通常のブラウザなら開く。"
          , "個別 /product.{番号}.html の JSON-LD に、価格と在庫がある。"
          ]
      }
  , (site "shopch" "ショップチャンネル" DirectAndSpecialty [KNew] Http)
      { searchNew = Just "https://www.shopch.jp/pc/product/prodlist/keyword?keyWord={q}&sort=2"
      , itemPatterns = ["^https://www\\.shopch\\.jp/pc/product/proddetail"]
      , hints = ["個別ページは、本文の「¥○○（税込）」が価格で、画像は og:image（L.jpg が大きい）。在庫と送料の額はHTMLから読めなかった。"]
      }
  , (site "panasonic" "パナソニック Store Plus" DirectAndSpecialty [KNew] BrowserNormal)
      { searchNote = Just "購入ページ：panasonic.jp/{分類}/products/{型番}/shopping.html"
      , itemPatterns = ["^https://panasonic\\.jp/.*/shopping", "^https://ec-plus\\.panasonic\\.jp/"]
      , hints =
          [ "製品ページの「公式通販サイトで購入」から入る。色別のページ（shopping.{型番-色}.html）に、税込価格・在庫状況・お届け目安・購入ボタンが出る。"
          , "ec-plus.panasonic.jp は、HTTP・ヘッドレスでは応答しない。製品ページの JSON-LD にある価格は参考値。"
          ]
      }
  , (site "balmuda" "BALMUDA公式" DirectAndSpecialty [KNew] Http)
      { searchNote = Just "購入ページ：store.balmuda.com/disp/CBlSfSelectGoodsPage.jsp?PRODUCT_SERIES={型番}"
      , itemPatterns = ["^https://store\\.balmuda\\.com/"]
      , hints = ["価格と在庫は、store 側の本文に色別で出る（「完売しました」など）。送料は3,300円以上で無料（11/20時点）。"]
      }
  , (site "cado" "cado公式" DirectAndSpecialty [KNew] Http)
      { searchNew = Just "https://cado.com/search?q={q}"
      , itemPatterns = ["^https://cado\\.com/products/"]
      , hints =
          [ "在庫は、本文の「売り切れ」の文字ではなく JSON-LD の offers.availability で判断する（在庫ありの商品にも、その文字が埋め込まれている）。"
          , "2026-11-21：送料は本文の「4,000円（税込）以上のご購入で送料無料。…送料は一律550円（税込）」。検索結果の値札にも「売り切れ」のひな形が入っているので、一覧では在庫を読まない。"
          ]
      }
  , (site "irisplaza" "アイリスプラザ" DirectAndSpecialty [KNew] (TierStopped "不正アクセスの件によりECサイトが一時停止（503）"))
      { hints = ["Yahoo!ショッピングの公式店（store.shopping.yahoo.co.jp/irisplaza/）で探せるが、値はその店のもの。"]
      }
  , (site "dyson" "ダイソン公式" DirectAndSpecialty [KNew] BrowserNormal)
      { itemPatterns = ["^https://www\\.dyson\\.co\\.jp/"]
      , hints =
          [ "HTTP・ヘッドレスは Cloudflare のブロックだが、通常のブラウザなら開く。"
          , "製品ページの JSON-LD に、価格・在庫がある。"
          ]
      }
  , (site "plywood" "plywood" DirectAndSpecialty [KNew] Http)
      { enc = EucJp
      , searchNew = Just "https://www.plywood.jp/shop/shopbrand.html?search={q}&sort=price"
      , itemPatterns = ["^https://www\\.plywood\\.jp/shopdetail/"]
      , hints = ["ページは EUC-JP。価格は本文と、メタタグ product:price:amount にある。"]
      }
  , (site "costco" "コストコ" DirectAndSpecialty [KNew] HttpItem)
      { searchNew = Just "https://www.costco.co.jp/search?text={q}&sort=price-asc"
      , itemPatterns = ["^https://www\\.costco\\.co\\.jp/.*/p/[0-9]+"]
      , hints =
          [ "検索結果はJSで描画されるので、ブラウザで開く。会員でなくても多くの商品は価格が見える（一部は「ログインして価格を表示」）。"
          , "個別ページは HTTP でも、JSON-LD に税込価格がある（@type は小文字の product）。送料は shippingDetails と、本文の「通常配送料込み」。「2個セット」などは、1個あたりの価格で比べる。"
          , "2026-11-21：検索は、ヘッドレスの agent-browser で開いて商品リンクを拾えた。"
          ]
      }
  , -- 家電量販 ---------------------------------------------------------------
    (site "yodobashi" "ヨドバシ.com" Electronics [KNew] BrowserNormal)
      { searchNew = Just "https://www.yodobashi.com/?word={q}&sorttyp=SELL_PRICE_ASC&discontinued=false"
      , itemPatterns = ["^https://www\\.yodobashi\\.com/product/[0-9]+"]
      , hints =
          [ "HTTP・ヘッドレスは HTTP/2 エラーで開かないが、通常のブラウザなら開く。"
          , "個別 /product/{番号}/ で、価格・ポイント・配達料・在庫・カートが読める。画像は og:image にある。"
          , "保存したHTMLには、おすすめや閲覧履歴の枠が混ざる。抽出では、主の商品の id（products_maintitle・js_scl_unitPrice・salesInfoTxt・js_deliveryPriceTD・js_m_submitRelated）だけを読む。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で開けて、HTMLを保存できた。配達先の地名が入るので、保存したHTMLを人に渡さない。"
          ]
      }
  , (site "biccamera" "ビックカメラ.com" Electronics [KNew] BrowserNormal)
      { enc = ShiftJis
      , searchNew = Just "https://www.biccamera.com/bc/category/?q={q}&sort=02"
      , itemPatterns = ["^https://www\\.biccamera\\.com/bc/item/[0-9]+"]
      , hints =
          [ "ヘッドレスでは開かない。検索語は Shift_JIS でエンコードする（UTF-8 だと文字化けして、別の結果になる）。"
          , "個別 /bc/item/{番号}/ で、価格・10%ポイント・送料・在庫・カートが読める。画像は image.biccamera.com/img/{番号}_A01.jpg（?sr. 以降を外すと大きい）。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）では「通信に問題があるためアクセスを遮断しました」と出て開けなかった。"
          ]
      }
  , (site "joshin" "ジョーシンweb" Electronics [KNew] TierBlocked)
      { hints = ["HTTP・ヘッドレス・通常のブラウザのどれも、Akamai の Access Denied だった。代わりに楽天の「Joshin web」店を見る。"]
      }
  , (site "joshin-rakuten" "ジョーシン（楽天店）" Electronics [KNew] Http)
      { adapter = "rakuten"
      , searchNew = Just "https://search.rakuten.co.jp/search/mall/{q}/?s=2&sid=206032"
      , itemPatterns = ["^https://item\\.rakuten\\.co\\.jp/jism/"]
      , hints = ["楽天の「Joshin web」店（rakuten.co.jp/jism/）。個別ページの価格・在庫が HTTP で読める。値は楽天店のもの。"]
      }
  , (site "kojima" "コジマネット" Electronics [KNew] BrowserJs)
      { searchNew = Just "https://www.kojima.net/ec/prod_list.html?keyword={q}&sort=4"
      , itemPatterns = ["^https://www\\.kojima\\.net/ec/prod_detail"]
      , hints =
          [ "HTTP は202で空になる。ヘッドレスでも、数秒待てば読める。価格・在庫・送料は本文にある。"
          , "画像URLの IMG_PATH_M を IMG_PATH_L に変えると、大きい画像になる。"
          ]
      }
  , (site "yamada" "ヤマダウェブコム" Electronics [KNew] BrowserNormal)
      { searchNote = Just "トップの検索窓から入る（/search/{語}/?category=all&searchbox=1）"
      , itemPatterns = ["^https://www\\.yamada-denkiweb\\.com/[0-9]+"]
      , hints =
          [ "ヘッドレスは HTTP/2 エラーになる。通常のブラウザでも、/search/{語}/ を直接開くと「アクセスが制限されました」になったが、検索窓から入ると検索できた。"
          , "個別 /{番号}/ で、価格・お届け日・カートが読める。送料の額は「配送料金について」のページにある。"
          , "抽出では、主の商品の欄（l-list-product-info-main）だけを読む。主の商品の価格の欄に「税込」の文字は無く、関連商品の欄には「¥…(税込)」と出る。サイトの価格は税込として読む（2026-11-21 に確かめた）。在庫は配送先のエリアごとの表示で、お届けの目安が出ていれば在庫あり。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で個別ページを開けて、HTMLを保存できた（検索は試していない）。配送先のエリアの地名が入るので、保存したHTMLを人に渡さない。"
          ]
      }
  , (site "ksdenki" "ケーズデンキ" Electronics [KNew] BrowserNormal)
      { searchNew = Just "https://www.ksdenki.com/search?keyword={q}&sort=4"
      , itemPatterns = ["^https://www\\.ksdenki\\.com/item\\?sku="]
      , hints =
          [ "個別 /item?sku={JAN} の JSON-LD に、価格・在庫がある。送料は本文に出る。"
          , "抽出では、itemPriceIncld（税込）と itemPriceBase（税別）、pdStock（在庫）、icoPostage（送料の札）、cmnBtnAddCart（カート）を読む。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で個別ページを開けて、HTMLを保存できた。"
          ]
      }
  , (site "nojima" "ノジマオンライン" Electronics [KNew] BrowserNormal)
      { searchNew = Just "https://online.nojima.co.jp/app/catalog/list/init?searchWord={q}"
      , itemPatterns = ["^https://online\\.nojima\\.co\\.jp/commodity/"]
      , hints =
          [ "ヘッドレスは Akamai の403だが、通常のブラウザで数秒待つと開く。"
          , "個別 /commodity/1/{JAN}/ で、税込価格・ポイント・送料・カートが読める。画像は JSON-LD にある。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）では、個別ページが Access Denied で開けなかった。"
          ]
      }
  , (site "edion" "エディオン" Electronics [KNew] (TierStopped "全機能がメンテナンス中（再開予定 11/21 10:00）"))
      { hints = ["代わりに楽天のエディオン店を見る。"]
      }
  , (site "edion-rakuten" "エディオン（楽天店）" Electronics [KNew] Http)
      { adapter = "rakuten"
      , searchNew = Just "https://search.rakuten.co.jp/search/mall/{q}/?s=2&sid=272415"
      , itemPatterns = ["^https://item\\.rakuten\\.co\\.jp/edion/"]
      , hints = ["個別ページの価格・送料・在庫が HTTP で読める。取り寄せ品は、OutOfStock でもボタンが有効なことがある。"]
      }
  , (site "sofmap" "ソフマップ" Electronics [KNew, KUsed] HttpItem)
      { enc = ShiftJis
      , searchNew = Just "https://www.sofmap.com/search_result.aspx?keyword={q}&order_by=PRICE_ASC&stk_flg=2"
      , searchUsed = Just "https://www.sofmap.com/search_result.aspx?keyword={q}&product_type=USED&order_by=PRICE_ASC&stk_flg=2"
      , itemPatterns = ["^https://www\\.sofmap\\.com/product_detail\\.aspx\\?sku="]
      , hints =
          [ "検索結果はJSで描画されるので、ブラウザで読む。"
          , "個別 product_detail.aspx?sku=… は HTTP でも読める。新品は JSON-LD に価格・在庫がある（中古は本文を読む）。HTMLは Shift_JIS。"
          , "画像は og:image ではなく image.sofmap.com/images/product/… から取る。"
          , "送料は本文の「送料 ¥0 (税込)」。JSON-LD は2つあり、1つ目は JSON として壊れている。"
          , "2026-11-21：検索は、ヘッドレスの agent-browser で開いて商品リンクを拾えた。"
          ]
      }
  , (site "bestdenki" "ベスト電器" Electronics [KNew] TierOutOfScope)
      { hints = ["自社の通販は無い。通販は Yahoo!ショッピングの店だけ。"]
      }
  , (site "donki" "ドン・キホーテ" Electronics [KNew] (TierStopped "公式通販 shop.donki.com はメンテナンスページに転送された"))
  , -- インテリア・雑貨 ---------------------------------------------------------
    (site "nitori" "ニトリネット" Interior [KNew] BrowserNormal)
      { searchNew = Just "https://www.nitori-net.jp/ec/search/?q={q}&sort=price-asc"
      , itemPatterns = ["^https://www\\.nitori-net\\.jp/ec/product/"]
      , hints =
          [ "HTTP・ヘッドレスは HTTP/2 エラーだが、通常のブラウザなら開く。"
          , "個別 /ec/product/{番号}/ で、価格・送料・カート・画像が読める。"
          , "抽出では、価格は cart-price の欄、送料は仕様の表の「送料」の行、カートは #p-addItem、名前・在庫・画像は JSON-LD（色ごとの Product）を読む。価格の欄に「税込」の文字は無い（比べてある以前の価格には「(税込)」と出る）。サイトの価格は税込として読む（2026-11-21 に確かめた）。"
          , "「個別送料」の商品は、合計が11,000円未満だと基本送料も加わる（基本送料の額はページに無い）。確かめたのは個別送料の商品だけ。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で検索と個別ページを開けて、HTMLを保存できた。お届け先の地名・受取店舗・レビューの投稿者名が入るので、保存したHTMLを人に渡さない。"
          ]
      }
  , (site "ikea" "IKEA" Interior [KNew] HttpItem)
      { searchNew = Just "https://www.ikea.com/jp/ja/search/?q={q}&sort=PRICE_LOW_TO_HIGH"
      , itemPatterns = ["^https://www\\.ikea\\.com/jp/ja/p/"]
      , hints =
          [ "検索結果はブラウザで描画される。個別ページは HTTP でも、JSON-LD に価格・在庫・画像がある。"
          , "送料と店舗の在庫は、郵便番号を入れないと出ない。"
          , "2026-11-21：検索は、ヘッドレスの agent-browser で開いて商品リンクを拾えた。"
          ]
      }
  , (site "muji" "無印良品ネットストア" Interior [KNew] (TierStopped "ネットストアの全サービスが停止（セキュリティ強化のため）"))
  , (site "loft" "ロフト" Interior [KNew] Http)
      { searchNew = Just "https://www.loft.co.jp/store/goods/search.aspx?search=x&keyword={q}&sort=price"
      , itemPatterns = ["^https://www\\.loft\\.co\\.jp/store/g/"]
      , hints = ["個別ページの JSON-LD に、価格・在庫がある。送料はご利用ガイドにある。"]
      }
  , (site "hands" "ハンズ" Interior [KNew] Http)
      { searchNew = Just "https://hands.net/search/?q={q}&rankBy=price(float)/jpy:ascending"
      , itemPatterns = ["^https://hands\\.net/goods/[0-9]+"]
      , hints =
          [ "JSON-LD は WebSite だけで、Product は無い。本文の価格・在庫を読む。価格が2つ出る商品は、どちらが今の価格かを確かめる。"
          , "サーバーが TLS の Extended Main Secret に対応していない。meguri は「あれば使う」設定でつなぐ。"
          , "2026-11-21：抽出では、#item-info の欄を読む。値下げ品は価格が2つ出て、いまの価格は normal の欄（検索結果に出る額と同じ）。商品番号は JAN。送料は「ネットストアなら合計5,000円(税込)以上で送料無料」（未満の額はページに無い）。"
          , "検索結果は #search-result-list の中だけ。その後ろに「今、コレ売れました」の枠（検索と関係のない商品）が並ぶ。0件のときは「検索条件と一致する商品が見つかりませんでした」。"
          ]
      }
  , (site "francfranc" "Francfranc" Interior [KNew] HttpItem)
      { searchNew = Just "https://francfranc.com/search?q={q}"
      , itemPatterns = ["^https://francfranc\\.com/products/"]
      , hints =
          [ "Shopify。個別ページの JSON-LD に、価格・在庫・画像がある（width=2000 で大きい）。安い順の指定は効かなかった。"
          , "2026-11-21：検索結果はJSで描画される。HTTP で取ったHTMLに並ぶ商品は Pickup・Ranking などの枠で、検索結果ではない（件数だけ、title の「検索結果◯件」で分かる）。"
          ]
      }
  , (site "actus" "ACTUS" Interior [KNew] HttpItem)
      { searchNew = Just "https://online.actus-interior.com/search/?q={q}&sortKey=price_asc"
      , itemPatterns = ["^https://online\\.actus-interior\\.com/item/"]
      , hints =
          [ "www.actus-interior.com は企業サイトで、通販は online. の方。"
          , "検索結果はブラウザで読む。個別 /item/{ID}/ は HTTP でも JSON-LD が読める。"
          ]
      }
  , (site "unico" "unico" Interior [KNew] HttpItem)
      { searchNew = Just "https://www.unico-fan.co.jp/search?q={q}&sort_by=price-ascending"
      , itemPatterns = ["^https://www\\.unico-fan\\.co\\.jp/products/"]
      , hints = ["ページが4〜5MBと重い。送料は注文画面まで出ない。予約販売の商品は「在庫あり」でも、発送日が決まっていないことがある。"]
      }
  , (site "idee" "IDÉE" Interior [KNew] HttpItem)
      { enc = ShiftJis
      , searchNew = Just "https://www.idee-online.com/shop/goods/search.aspx?search=x&keyword={q}"
      , itemPatterns = ["^https://www\\.idee-online\\.com/shop/g/"]
      , hints = ["Shift_JIS なので、UTF-8 の検索語では0件になる。JSON-LD は無いので、本文を読む。"]
      }
  , (site "hokuohkurashi" "北欧、暮らしの道具店" Interior [KNew] BrowserNormal)
      { searchNote = Just "個別：/product_contents/{番号}（一覧からたどる）"
      , itemPatterns = ["^https://hokuohkurashi\\.com/product_contents/"]
      , hints =
          [ "HTTP・ヘッドレスは CloudFront の403だが、通常のブラウザなら開く。"
          , "個別ページで、税込価格・カート・画像（imgix）が読める。"
          ]
      }
  , -- ホームセンター・法人通販 -------------------------------------------------
    (site "cainz" "カインズ" HomeCenter [KNew] Http)
      { searchNew = Just "https://www.cainz.com/search/?q={q}&sortRule=lowPrice"
      , itemPatterns = ["^https://www\\.cainz\\.com/g/[0-9]+\\.html"]
      , hints = ["個別 /g/{JAN}.html の JSON-LD に、価格・在庫がある。在庫と最大注文数は、受け取る店舗で変わる。"]
      }
  , (site "kohnan" "コーナン" HomeCenter [KNew] Http)
      { searchNew = Just "https://www.kohnan-eshop.com/shop/goods/search.aspx?keyword={q}&search=x&seq=gp"
      , itemPatterns = ["^https://www\\.kohnan-eshop\\.com/shop/g/"]
      , hints = ["安い順は seq=gp（sort=price は効かない）。「店舗受取のみ」「メーカー直送」の注意書きを読む。"]
      }
  , (site "dcm" "DCM" HomeCenter [KNew] (TierStopped "全面休止（再開日は未定）"))
  , (site "vivahome" "ビバホーム" HomeCenter [KNew] HttpItem)
      { searchNew = Just "https://www.vivahome.com/search/?q={q}&srule=price-low-to-high"
      , itemPatterns = ["^https://www\\.vivahome\\.com/[a-z0-9-]+/.+/[0-9]+\\.html"]
      , hints =
          [ "通販は「ビバムサシオンライン」。JSON-LD の価格は税抜なので、税込は本文の「Web販売価格」を使う。"
          , "HTTP で取ったHTMLには「売り切れ」の文言が隠れて入っているので、在庫は画面で見る。"
          ]
      }
  , (site "monotaro" "モノタロウ" HomeCenter [KNew] BrowserNormal)
      { searchNew = Just "https://www.monotaro.com/s/?q={q}"
      , itemPatterns = ["^https://www\\.monotaro\\.com/[gp]/"]
      , hints =
          [ "HTTP・ヘッドレスは Akamai の Access Denied だが、通常のブラウザなら開く。"
          , "個別 /g/{番号}/ で、税込価格・出荷目安・「バスケットに入れる」・大きい画像（/pi/full/）が読める。"
          ]
      }
  , (site "askul" "ASKUL" HomeCenter [KNew] BrowserJs)
      { searchNew = Just "https://www.askul.co.jp/ksearch/?searchWord={q}&sortDir=1"
      , itemPatterns = ["^https://www\\.askul\\.co\\.jp/p/"]
      , hints = ["HTTP は403だが、ヘッドレスなら読める。ログインなしで税込価格と在庫が見える。個人が買えるかは確かめていない。"]
      }
  , (site "tanomail" "たのめーる" HomeCenter [KNew] Http)
      { searchNew = Just "https://www.tanomail.com/search/res/{q}/"
      , itemPatterns = ["^https://www\\.tanomail\\.com/product/(variation/)?[0-9]+/"]
      , hints = ["ログインなしで、税込価格・在庫・ポイントが見える。画像URLの ?sr.dw= を外すと原寸になる。"]
      }
  , (site "kaunet" "カウネット" HomeCenter [KNew] Http)
      { enc = ShiftJis
      , searchNew = Just "https://www.kaunet.com/rakuraku/spook3/main?ShowList=1&Keyword={q}&SortOrder=Ascending"
      , itemPatterns = ["^https://www\\.kaunet\\.com/(rakuraku/variation|kaunet/goods)/[A-Za-z0-9]+/"]
      , hints = ["取扱いが終わった商品は、JSON-LD の在庫が画面と食い違う。本文の表示で判断する。"]
      }
  , (site "amazon-business" "Amazonビジネス" HomeCenter [KNew] TierOutOfScope)
      { hints = ["ログインしないと価格が出ない。"]
      }
  , -- フリマ・オークション・個人間 ---------------------------------------------
    (site "mercari" "メルカリ" FleaAuction [KUsed, KP2P] BrowserJs)
      { searchUsed = Just "https://jp.mercari.com/search?keyword={q}&status=on_sale&sort=price&order=asc"
      , itemPatterns = ["^https://jp\\.mercari\\.com/item/m[0-9]+"]
      , hints =
          [ "JSで描画されるので、HTTP では価格が取れない。ブラウザで開くと、描画後の JSON-LD に価格・在庫が入る。"
          , "売却済みの相場は status=sold_out。"
          , "送料・状態・購入の入口は、data-testid（配送料の負担・商品の状態・checkout-button）の欄にある。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で検索と個別ページを開けて、HTMLを保存できた。"
          ]
      }
  , (site "mercari-shops" "メルカリShops" FleaAuction [KNew, KUsed] BrowserJs)
      { searchNote = Just "メルカリの検索結果に混ざって出る"
      , itemPatterns = ["^https://jp\\.mercari\\.com/shops/product/"]
      , hints = ["個別 /shops/product/{id} もJSで描画される。送料は購入者負担で、「¥480~3,000」のように幅でしか出ない。"]
      }
  , (site "rakuma" "ラクマ" FleaAuction [KUsed, KP2P] Http)
      { searchUsed = Just "https://fril.jp/s?query={q}&transaction=selling&sort=sell_price&order=asc"
      , itemPatterns = ["^https://item\\.fril\\.jp/[0-9a-f]+"]
      , hints = ["個別は item.fril.jp/{id}。売り切れても JSON-LD は InStock のままなので、本文の「SOLD OUT」と「購入に進む」の有無で判断する。"]
      }
  , (site "paypayfleamarket" "Yahoo!フリマ（旧PayPayフリマ）" FleaAuction [KUsed, KP2P] Http)
      { searchUsed = Just "https://paypayfleamarket.yahoo.co.jp/search/{q}?open=1&sort=price&order=asc"
      , itemPatterns = ["^https://paypayfleamarket\\.yahoo\\.co\\.jp/item/z[0-9]+"]
      , hints =
          [ "検索結果の商品は、HTMLの <a href=\"/item/z…\"> にある（__NEXT_DATA__ ではない）。個別ページは、JSON-LD で価格・在庫・送料が読める。"
          , "英字の型番で探すと無関係の商品が混ざるので、商品名で見分ける。"
          ]
      }
  , (site "yahuoku" "ヤフオク" FleaAuction [KUsed, KAuction] Http)
      { searchUsed = Just "https://auctions.yahoo.co.jp/search/search?p={q}&s1=cbids&o1=a"
      , itemPatterns = ["^https://auctions\\.yahoo\\.co\\.jp/jp/auction/[a-z0-9]+"]
      , hints =
          [ "__NEXT_DATA__ に、現在価格・入札数・即決価格・終了日時がある。"
          , "落札相場：auctions.yahoo.co.jp/closedsearch/closedsearch?p={語}（meguri sold）。"
          , "荷物のサイズを決めていない出品は、送料が「落札者負担」とだけ出て、額が分からない。"
          ]
      }
  , (site "mbok" "モバオク" FleaAuction [KUsed, KAuction] Http)
      { enc = ShiftJis
      , searchUsed = Just "https://www.mbok.jp/_l?q={q}&o=1"
      , itemPatterns = ["^https://www\\.mbok\\.jp/item/item_[0-9]+"]
      , hints = ["ページも検索語も Shift_JIS。型番で探すと0件になりやすい。"]
      }
  , (site "aucfan" "オークファン" FleaAuction [KInfo] BrowserJs)
      { searchUsed = Just "https://aucfan.com/search1/q-{q}/"
      , hints = ["ログインなしで、直近30日・9ページまでの落札相場が見られる。JSで描画される。"]
      }
  , (site "jmty" "ジモティー" FleaAuction [KUsed, KP2P] Http)
      { searchUsed = Just "https://jmty.jp/all/sale/search?keyword={q}"
      , itemPatterns = ["^https://jmty\\.jp/[a-z]+/sale-[a-z]+/article-[a-z0-9]+"]
      , hints =
          [ "都道府県で絞るなら /{pref}/sale/search。sale-all?keyword= ではキーワードが効かない。"
          , "「受付終了投稿一覧」の見出しより後ろは受付終了なので除く。問い合わせにはログインが要る。"
          ]
      }
  , (site "facebook-marketplace" "Facebook Marketplace" FleaAuction [KP2P] TierOutOfScope)
      { hints = ["ログインしないと場所が米国に固定され、日本の出品が出ない。"]
      }
  , (site "oikura" "おいくら" FleaAuction [KInfo] TierOutOfScope)
      { hints = ["買取の一括査定サイトで、買うことはできない。売るときの相場の参考にはなる。"]
      }
  , -- リユース店 -------------------------------------------------------------
    (site "2ndstreet" "2nd STREET" Reuse [KUsed] BrowserNormal)
      { searchUsed = Just "https://www.2ndstreet.jp/search?keyword={q}&sortBy=price&sortOrder=asc"
      , itemPatterns = ["^https://www\\.2ndstreet\\.jp/goods/detail/goodsId/[0-9]{13}"]
      , hints =
          [ "HTTP・ヘッドレスは Akamai の Access Denied だが、通常のブラウザなら開く。"
          , "個別 /goods/detail/goodsId/{13桁}/shopsId/{店} は、JSON-LD に品番・画像・状態がある。本文には状態ランク・店舗・送料（11/20時点で1注文770円）が出る。"
          , "主の商品は #goodsDescription と #price。おすすめの枠（itemCard）にも同じ形の値が並ぶので、読まない。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で検索と個別ページを開けて、HTMLを保存できた。"
          ]
      }
  , (site "2ndstreet-rakuten" "2nd STREET（楽天店）" Reuse [KUsed] Http)
      { adapter = "rakuten"
      , searchUsed = Just "https://search.rakuten.co.jp/search/mall/{q}/?s=2&sid=264046"
      , itemPatterns = ["^https://item\\.rakuten\\.co\\.jp/jumblestore/[0-9]{13}"]
      , hints = ["item.rakuten.co.jp/jumblestore/{13桁}/。同じ13桁の商品番号で、自社EC・Yahoo!店と同じ在庫を引ける（価格・送料も一致した）。"]
      }
  , (site "2ndstreet-yahoo" "2nd STREET（Yahoo!店）" Reuse [KUsed] Http)
      { adapter = "yahoo-shopping"
      , searchUsed = Just "https://store.shopping.yahoo.co.jp/ssol-shopping/search.html?p={q}"
      , itemPatterns = ["^https://store\\.shopping\\.yahoo\\.co\\.jp/ssol-shopping/[0-9]{13}\\.html"]
      , hints = ["store.shopping.yahoo.co.jp/ssol-shopping/{13桁}.html。商品番号は自社EC・楽天店と共通。"]
      }
  , (site "hardoff" "ハードオフ（オフモール）" Reuse [KUsed] Http)
      { searchUsed = Just "https://netmall.hardoff.co.jp/search/?q={q}&s=3"
      , itemPatterns = ["^https://netmall\\.hardoff\\.co\\.jp/product/[0-9]+"]
      , hints =
          [ "0件のときは HTTP 404 が返る。/cate/?keyword= は使えない。"
          , "個別ページの JSON-LD の価格は税抜。税込価格とランクは本文にある。"
          , "送料は POST /ajax/CHfOmCalcDelvCom_J.jsp（toPref＝都道府県名、goodsNo）で、都道府県ごとに取れる。"
          ]
      }
  , (site "treasure-factory" "トレジャーファクトリー" Reuse [KUsed] Http)
      { searchUsed = Just "https://ec.treasure-f.com/search?word={q}&order=lowprice&step=1"
      , itemPatterns = ["^https://ec\\.treasure-f\\.com/item/[0-9]+"]
      , hints =
          [ "keyword= だと全件が出る。0件でも200が返る。"
          , "送料は /item/postage?code={商品番号}&prefId={都道府県コード} で取れる。画像URLの /450/ を /1500/ にすると大きい。"
          ]
      }
  , (site "bookoff" "ブックオフ オンライン" Reuse [KNew, KUsed] Http)
      { searchUsed = Just "https://shopping.bookoff.co.jp/search/keyword/{q}?sort=50"
      , itemPatterns = ["^https://shopping\\.bookoff\\.co\\.jp/(used|new)/[0-9]+"]
      , hints = ["家電は扱っていない。在庫なしの商品も混ざるので、「在庫あり」の表示を見る。"]
      }
  , (site "geo" "ゲオオンラインストア" Reuse [KUsed] BrowserNormal)
      { enc = ShiftJis
      , searchUsed = Just "https://ec.geo-online.co.jp/shop/goods/search.aspx?search=x&keyword={q}&sort=sp"
      , itemPatterns = ["^https://ec\\.geo-online\\.co\\.jp/shop/g/"]
      , hints =
          [ "HTTP・ヘッドレスは Akamai に拒否されるが、通常のブラウザなら開く。検索語は Shift_JIS（UTF-8 だと文字化けする）。"
          , "個別 /shop/g/g{番号}/ で、状態ランク・税込価格・送料の条件・保証・カートが読める。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）では「ただいまサイトが大変込み合っております」の案内が出て、検索を開けなかった。"
          ]
      }
  , (site "komehyo" "KOMEHYO" Reuse [KUsed] BrowserNormal)
      { searchUsed = Just "https://komehyo.jp/search/?q={q}"
      , itemPatterns = ["^https://komehyo\\.jp/product/"]
      , hints =
          [ "HTTP・ヘッドレスはエラーページに飛ばされるが、通常のブラウザなら開く。"
          , "個別 /product/{番号}/ で、税込価格・商品ランク・在庫店舗・付属品・カートが読める。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）では、検索は開けたが、個別ページは「セッションが無効となっています」のエラーページになった。"
          ]
      }
  , (site "okoku" "買取王国" Reuse [KUsed] Http)
      { searchUsed = Just "https://www.okoku.jp/search?q={q}&sort_by=price-ascending&filter.v.availability=1"
      , itemPatterns = ["^https://(www\\.)?okoku\\.jp/products/"]
      , hints =
          [ "Shopify。個別ページの JSON-LD に、価格・在庫がある。"
          , "2026-11-21：ランクは本文（c_main-product__rank-line）。送料は商品ページに無い。"
          ]
      }
  , (site "wonderrex" "ワンダーレックス" Reuse [KUsed] Http)
      { searchUsed = Just "https://wonderrex-online.com/search?q={q}&sort_by=price-ascending"
      , itemPatterns = ["^https://wonderrex-online\\.com/products/"]
      , hints = ["wonderrex.jp は会社のサイト。送料表は /policies/shipping-policy にある。売り切れの商品も混ざる。"]
      }
  , (site "recyclemart" "リサイクルマート" Reuse [KUsed] TierOutOfScope)
      { hints = ["店頭だけで、ネット販売は無い。"]
      }
  , -- 海外・レンタル ---------------------------------------------------------
    (site "ebay" "eBay" OverseasRental [KNew, KUsed] BrowserNormal)
      { searchNew = Just "https://www.ebay.com/sch/i.html?_nkw={q}&_sop=15"
      , itemPatterns = ["^https://www\\.ebay\\.com/itm/[0-9]+"]
      , hints =
          [ "HTTP は403、ヘッドレスはエラーだが、通常のブラウザなら開く。"
          , "個別 /itm/{番号} で、円に換算した価格・送料の欄・カートが読める。画像は s-l400 を s-l1600 にすると大きい。関税や通関の注意書きも読む。"
          , "抽出では、価格（x-price-primary と、円の換算の x-price-approx）、送料の欄、数量、出品者、「今すぐ買う」のボタンを読む。円の額は目安の換算で、関税・輸入消費税・手数料は別なので、税込価格と支払額は出さない（換算の額は税抜の欄に入れる）。"
          , "検索結果には /itm/123456 のような飾りのリンクが混ざる。商品のリンクは www.ebay.com/itm/{12桁前後}。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で検索と個別ページを開けて、HTMLを保存できた。"
          ]
      }
  , (site "amazon-com" "Amazon.com" OverseasRental [KNew] BrowserJs)
      { itemPatterns = ["^https://www\\.amazon\\.com/(.*/)?dp/[A-Z0-9]{10}"]
      , hints = ["検索は amazon.co.jp に転送される。/dp/{ASIN} を開くと、日本へ配送できるかが出る（確かめた4商品はすべて配送不可で、送料と関税は確かめられていない）。"]
      }
  , (site "aliexpress" "AliExpress" OverseasRental [KNew] BrowserJs)
      { searchNew = Just "https://ja.aliexpress.com/w/wholesale-{q}.html"
      , itemPatterns = ["^https://ja\\.aliexpress\\.com/item/"]
      , hints = ["HTTP は確認ページに飛ばされる。ブラウザなら、個別ページの JSON-LD（offers.price）と本文の送料が読める。店名は取れなかった。"]
      }
  , (site "buyma" "BUYMA" OverseasRental [KNew] Http)
      { searchNew = Just "https://www.buyma.com/r/-O3/{q}/"
      , itemPatterns = ["^https://www\\.buyma\\.com/item/[0-9]+"]
      , hints =
          [ "個別ページの JSON-LD（hasVariant[].offers）に、価格・在庫・色別がある。"
          , "送料は、JSON-LD が0円でも本文が「送料着払」のことがあるので、本文を見る。"
          , "2026-11-21：価格に「税込」の文字は無い。「関税負担なし」の印が付いた商品は追加料金の負担がない（/contents/duty/ の案内。国内発送・関税元払い・発生時に返金）ので、印があるときだけ表示価格を支払う額として読む。印が無い海外発送の商品は、関税・輸入消費税が別にかかることがある。"
          , "「この商品は、売り切れました」は隠してあるひな形で、在庫ありの商品にも入っている。出品者（パーソナルショッパー）は js-itemdetail-shopper-name。"
          , "検索で該当が無いと、HTTP 404 で「お探しの条件にあてはまる商品は見つかりませんでした」と出て、その下に検索と関係のない商品が並ぶ。"
          ]
      }
  , (site "rentio" "Rentio" OverseasRental [KRental] Http)
      { searchNew = Just "https://www.rentio.jp/products?keywords={q}&sorting=ascend_by_master_price"
      , itemPatterns = ["^https://www\\.rentio\\.jp/products/"]
      , hints = ["JSON-LD の lowPrice は最安の月額。ワンタイムの料金と配送手数料は本文に、在庫はお届け日のカレンダーにある。"]
      }
  , (site "clas" "CLAS" OverseasRental [KRental] HttpItem)
      { searchNew = Just "https://clas.style/search?q={q}"
      , itemPatterns = ["^https://clas\\.style/[0-9]+"]
      , hints = ["検索結果はブラウザで描画される。個別 /{番号} は HTTP でも、月額・在庫・配送できるかが読める。"]
      }
  , -- 価格以外の情報源 -------------------------------------------------------
    (site "recall-caa" "消費者庁リコール情報サイト" NonPrice [KInfo] Http)
      { searchNote = Just "recall.caa.go.jp/result/index.php に POST（search={語}&screenkbn=01&category=0）"
      , hints = ["詳細 detail.php?rcl={番号} の型番・販売期間・対応は、<script> 内の contentsText の JSON に入っている。"]
      }
  , (site "recall-cpsc" "CPSC" NonPrice [KInfo] Http)
      { searchNote = Just "API：saferproducts.gov/RestWebServices/Recall?format=json&ProductName={語}"
      , hints = ["サイトの検索は HTTP では絞り込まれないので、公式APIを使う。"]
      }
  , -- 品目別の専門店 ---------------------------------------------------------
    (site "mapcamera" "マップカメラ" ItemSpecific [KNew, KUsed] BrowserNormal)
      { searchNew = Just "https://www.mapcamera.com/search?keyword={q}"
      , itemPatterns = ["^https://www\\.mapcamera\\.com/item/[0-9]+"]
      , hints =
          [ "HTTP・ヘッドレスは Akamai の403だが、通常のブラウザなら開く。"
          , "個別 /item/{番号} で、税込価格・付属品・カートが読める。価格は JSON-LD にもある。画像は …/1/l.jpg。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）では、検索は開けたが、個別ページは Access Denied だった。"
          ]
      }
  , (site "kitamura" "カメラのキタムラ" ItemSpecific [KNew, KUsed] BrowserJs)
      { searchNew = Just "https://shop.kitamura.jp/ec/list?keyword={q}&sort=price"
      , itemPatterns = ["^https://shop\\.kitamura\\.jp/ec/(pd|used)/"]
      , hints =
          [ "SPA なので、HTTP では空になる。ブラウザで2〜3秒待つと、中古 /ec/used/{管理コード} の状態ランク・付属品・保証まで読める。"
          , "中古の一覧：?keyword3={JAN}&type=u"
          ]
      }
  , (site "janpara" "じゃんぱら" ItemSpecific [KUsed] BrowserNormal)
      { searchUsed = Just "https://www.janpara.co.jp/sale/search/result/?KEYWORDS={q}"
      , itemPatterns = ["^https://www\\.janpara\\.co\\.jp/sale/search/detail/"]
      , hints =
          [ "HTTP・ヘッドレスは WAF に拒否されるが、通常のブラウザなら開く。"
          , "詳細 /sale/search/detail/?ITMCODE={番号} に、在庫のある店舗ごとの価格と状態が並ぶ。og:image は分類の画像なので、商品の画像は本文から取る。"
          , "抽出では、itemdetail_table の行（店舗・状態・価格）を読む。行が複数あれば、いちばん安い行。表の見出しが「価格（税込）」。送料は画像（送料メーター）で出るので読めない。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）で検索と個別ページ（?SRCODE={商品番号}）を開けて、HTMLを保存できた。"
          ]
      }
  , (site "iosys" "イオシス" ItemSpecific [KNew, KUsed] Http)
      { searchUsed = Just "https://iosys.co.jp/items?q={q}&sort=l"
      , itemPatterns = ["^https://iosys\\.co\\.jp/items/.+/[0-9]+$"]
      , hints = ["JSON-LD に、価格・在庫・状態がある。検索語の一致が緩いので、型番まで入れる。送料の表示は無い。"]
      }
  , (site "ishibashi" "イシバシ楽器" ItemSpecific [KNew, KUsed] Http)
      { searchNew = Just "https://store.ishibashi.co.jp/search?q={q}&sort=price"
      , searchUsed = Just "https://store.ishibashi.co.jp/search?q={q}&sort=price&s4%5B0%5D=%E4%B8%AD%E5%8F%A4"
      , itemPatterns = ["^https://store\\.ishibashi\\.co\\.jp/(view/item|ec/product)/"]
      , hints =
          [ "JSON-LD に価格・在庫がある。中古はランクと付属品が読める。売れた後も掲載が残ることがある。"
          , "2026-11-21：MakeShop。本文に「¥ 110 税込」「配送料: ¥ 990」があり、店舗とランクは im-product-rank の data-name（「横浜店 S：新品」）。商品コードは「09-{JAN}」の形のことがある。"
          , "0件のときは、件数の欄（result-info-hitnum）が 0 になる。中古の絞り込みの s4[0] は、% エンコードして渡す。"
          ]
      }
  , (site "digimart" "デジマート" ItemSpecific [KNew, KUsed] BrowserNormal)
      { searchNew = Just "https://www.digimart.net/search?keywordAnd={q}"
      , itemPatterns = ["^https://www\\.digimart\\.net/cat[0-9]+/shop[0-9]+/"]
      , hints =
          [ "HTTP・ヘッドレスは Cloudflare の確認画面だが、通常のブラウザなら開く。"
          , "個別 /cat01/shop{n}/DS{n}/ で、税込価格・送料・状態・出品店が読める。"
          ]
      }
  , (site "babiesrus" "ベビーザらス" ItemSpecific [KNew] BrowserNormal)
      { searchNew = Just "https://www.toysrus.co.jp/ja-jp/search/?q={q}"
      , itemPatterns = ["^https://www\\.(toysrus|babiesrus)\\.co\\.jp/ja-jp/.*[0-9]+\\.html"]
      , hints =
          [ "babiesrus.co.jp に転送される。HTTP・ヘッドレスはボット確認で止まった。通常のブラウザでは、操作しなくても自動で通過して開いた。"
          , "個別 /ja-jp/{番号}.html の JSON-LD に、価格・在庫がある。"
          ]
      }
  , (site "akachan" "赤ちゃん本舗" ItemSpecific [KNew] Http)
      { searchNew = Just "https://shop.akachan.jp/shop/goods/search.aspx?search=x&keyword={q}&sort=sp"
      , itemPatterns = ["^https://shop\\.akachan\\.jp/shop/g/"]
      , hints =
          [ "www.akachan.jp は価格のないカタログ。通販の個別ページは、JSON-LD に価格・在庫がある。"
          , "2026-11-21：送料は本文の「一般商品 600円（税込）…5,500円（税込）以上で送料当社負担」。JAN は本文の「JANコード：」にあり、JSON-LD の mpn は店の商品番号。店舗ごとの在庫の一覧はネットの在庫ではない。"
          ]
      }
  , (site "zozotown" "ZOZOTOWN" ItemSpecific [KNew, KUsed] BrowserNormal)
      { enc = ShiftJis
      , searchNew = Just "https://zozo.jp/search/?p_keyv={q}"
      , itemPatterns = ["^https://zozo\\.jp/shop/[^/]+/goods/[0-9]+"]
      , hints =
          [ "HTTP・ヘッドレスは応答しないが、通常のブラウザなら開く。検索語は Shift_JIS（UTF-8 だと文字化けして0件になる）。"
          , "個別 /shop/{店}/goods/{番号}/ で、サイズ別の在庫・税込価格・カートが読める。JSON-LD の在庫は、サイズ別の表示と食い違うことがある。"
          , "2026-11-21：画面ありの agent-browser（ログインなし）では、検索が Access Denied だった。"
          ]
      }
  , (site "locondo" "ロコンド" ItemSpecific [KNew] HttpItem)
      { searchNew = Just "https://www.locondo.jp/shop/search?keyword={q}&alignmentSequence=price%20asc"
      , itemPatterns = ["^https://www\\.locondo\\.jp/shop/commodity/"]
      , hints =
          [ "検索結果はJSで描画される。個別ページは HTTP でも、JSON-LD の価格とサイズ別の在庫が読める。"
          , "2026-11-21：店は本文の「ショップ：」の欄。送料は商品ページに無い。"
          ]
      }
  , (site "rakuten-books" "楽天ブックス" ItemSpecific [KNew] BrowserJs)
      { searchNew = Just "https://books.rakuten.co.jp/search?sitem={q}&s=4"
      , itemPatterns = ["^https://books\\.rakuten\\.co\\.jp/rb/[0-9]+"]
      , hints = ["検索は HTTP で取れる。個別 /rb/{番号}/ は HTTP だと「見つかりません」が返るので、ブラウザで開く。"]
      }
  , (site "honto" "honto" ItemSpecific [KNew] Http)
      { searchNew = Just "https://honto.jp/netstore/search.html?k={q}"
      , itemPatterns = ["^https://honto\\.jp/(netstore|ebook)/pd"]
      , hints = ["電子書籍だけを扱う。紙の本は、連携書店の外部サイトに送られる。"]
      }
  , (site "kinokuniya" "紀伊國屋書店ウェブストア" ItemSpecific [KNew] HttpItem)
      { searchNote = Just "トップの検索窓から入る（POST）。検索はURLに語が残らない。安い順の並べ替えは無い"
      , itemPatterns = ["^https://www\\.kinokuniya\\.co\\.jp/f/dsg-"]
      , hints = ["個別 /f/dsg-01-{ISBN} は HTTP でも、価格・在庫冊数・ポイント・大きい表紙画像が読める。"]
      }
  ]
