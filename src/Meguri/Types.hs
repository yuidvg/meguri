{-# LANGUAGE DeriveAnyClass #-}

-- | 値の型。値ごとに出どころ（'Field'）を持たせる。
--
-- 確認済みの値は、個別ページからの抽出でしか作れない。
-- 一覧やAPIの値は @Offer OnList@、個別ページの値は @Offer OnPage@ で、
-- 確認済みの判定（"Meguri.Confirm"）は後者しか受け取らない。
module Meguri.Types
  ( Yen (..)
  , unYen
  , Shipping (..)
  , shippingCost
  , Stock (..)
  , BuyEntry (..)
  , Method (..)
  , OnPage (..)
  , OnList (..)
  , Field (..)
  , got
  , gotRaw
  , missing
  , orField
  , withNote
  , mapSource
  , Keys (..)
  , noKeys
  , Offer (..)
  , emptyOffer
  , payable
  , Sighting (..)
  , Via (..)
  , Reading (..)
  , Market (..)
  , Confirmation (..)
    -- * 取りに行った結果
  , Role (..)
  , Outcome (..)
  , Block (..)
  , Listed (..)
  , Attempt (..)
  , fine
  , troubles
  , failureKind
  , Skip (..)
  , skipKind
  , Did (..)
  , attemptsOf
  , Coverage (..)
  , Status (..)
  , statusOf
  , statusKind
  ) where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON, object, (.=))
import Data.Aeson.Types (Pair)
import Protolude

newtype Yen = Yen Int
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Num, ToJSON, FromJSON)

unYen :: Yen -> Int
unYen (Yen n) = n

-- | 送料。額が決まらないもの（配送先しだい・購入者負担）は、支払額を出せない。
data Shipping
  = Free
  | Fixed Yen
  | -- | @FreeOver しきい値 送料@：しきい値以上で無料、未満なら送料がかかる
    FreeOver Yen Yen
  | ByDestination
  | BuyerPays
  | ShipUnknown Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 税込価格に対する送料。決まらなければ 'Nothing'。
shippingCost :: Yen -> Shipping -> Maybe Yen
shippingCost price = \case
  Free -> Just 0
  Fixed y -> Just y
  FreeOver threshold fee -> Just (if price >= threshold then 0 else fee)
  ByDestination -> Nothing
  BuyerPays -> Nothing
  ShipUnknown _ -> Nothing

data Stock
  = InStock
  | Backorder
  | OutOfStock
  | -- | 中古・個人間で販売中
    Selling
  | Sold
  | Auction {bids :: Int, endsAt :: Text}
  | StockUnknown
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 購入の入口（カートや購入のボタン）。押さない。
data BuyEntry = BuyActive Text | BuyDisabled Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | ページのどこから読んだか
data Method
  = JsonLd
  | Microdata
  | OpenGraph
  | NextData
  | DataAttr
  | EmbeddedJson
  | Body
  | -- | そのサイトの送料計算（配送先を渡して問い合わせた）
    ShippingCalc
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 個別ページだけが持てる出どころ（HTTP で取った個別ページの、どこから読んだか）
newtype OnPage = ItemPage Method
  deriving stock (Eq, Show, Generic)

-- JSON は {"tag":"ItemPage","contents":…} の形のままにする
instance ToJSON OnPage where
  toJSON = genericToJSON tagged
  toEncoding = genericToEncoding tagged

instance FromJSON OnPage where
  parseJSON = genericParseJSON tagged

tagged :: Options
tagged = defaultOptions {tagSingleConstructors = True}

-- | 一覧・APIの出どころ（確認済みにはならない）
data OnList = SearchList | Api Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 出どころつきの値。読めなかったら @value = Nothing@ で、@note@ に理由を書く。
data Field s a = Field
  { value :: Maybe a
  , source :: s
  , raw :: Maybe Text
  , note :: Maybe Text
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (ToJSON, FromJSON)

got :: s -> a -> Field s a
got s a = Field (Just a) s Nothing Nothing

gotRaw :: s -> Text -> a -> Field s a
gotRaw s r a = Field (Just a) s (Just r) Nothing

missing :: s -> Text -> Field s a
missing s why = Field Nothing s Nothing (Just why)

-- | 左に値があれば左、無ければ右
orField :: Field s a -> Field s a -> Field s a
orField a b = if isJust a.value then a else b

withNote :: Text -> Field s a -> Field s a
withNote n f = f {note = Just (maybe n (\old -> old <> "／" <> n) f.note)}

mapSource :: (s -> t) -> Field s a -> Field t a
mapSource f (Field v s r n) = Field v (f s) r n

-- | 同じ物かを見分ける手がかり
data Keys = Keys
  { jan :: Maybe Text
  , model :: Maybe Text
  , siteItemId :: Maybe Text
  , -- | 複数の窓口で共通の在庫番号。
    -- 2nd STREET の13桁の商品番号（自社EC・楽天店・Yahoo!店で共通）、
    -- Yahoo!フリマの z… の番号（ヤフオクの検索にも同じ出品が出る）
    sharedStockId :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

noKeys :: Keys
noKeys = Keys Nothing Nothing Nothing Nothing

-- | 1つの売り物について読めた値
data Offer s = Offer
  { site :: Text
  , url :: Text
  , name :: Field s Text
  , priceIncl :: Field s Yen
  , priceExcl :: Field s Yen
  , shipping :: Field s Shipping
  , stock :: Field s Stock
  , seller :: Field s Text
  , image :: Field s Text
  , buy :: Field s BuyEntry
  , -- | ログインしていない状態の値（参考）
    points :: Field s Int
  , condition :: Field s Text
  , keys :: Keys
  , -- | 取得した時刻（JST）
    at :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyOffer :: forall s. s -> Text -> Text -> Text -> Offer s
emptyOffer s site url at =
  Offer
    { site
    , url
    , name = no
    , priceIncl = no
    , priceExcl = no
    , shipping = no
    , stock = no
    , seller = no
    , image = no
    , buy = no
    , points = no
    , condition = no
    , keys = noKeys
    , at
    }
 where
  no :: forall a. Field s a
  no = missing s "読めなかった"

-- | 支払額＝税込価格＋送料。どちらかが分からなければ 'Nothing'。
payable :: Offer s -> Maybe Yen
payable o = do
  p <- o.priceIncl.value
  s <- o.shipping.value
  (p +) <$> shippingCost p s

data Market = New | Used
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 一覧やAPIから作った候補（未確認）
data Sighting = Sighting
  { term :: Text
  , market :: Market
  , offer :: Offer OnList
  , -- | どのページで見かけたか
    via :: Via
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | 売り物を見かけたページ：役割（検索の1ページ目・続き・語を広げた検索）とURL
data Via = Via
  { role :: Role
  , url :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON Via where
  toJSON v = object (rolePairs v.role <> ["url" .= v.url])

-- | 個別ページから作った記録
data Reading = Reading
  { offer :: Offer OnPage
  , -- | サイトのアダプタで読んだか（汎用の抽出だけなら確認済みにしない）
    adapter :: Bool
  , confirmation :: Confirmation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Confirmation
  = Confirmed
  | -- | 足りない項目
    Partial [Text]
  | Unconfirmed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 取りに行った結果 ------------------------------------------------------------
--
-- 結果は、した作業と同じ形の木にする。失敗は、それが起きた単位にだけ書く。
--
-- @
-- 検索1回（'Coverage'）・個別ページ1つ ─ 取りに行かなかった（'Skip'）
--                                     └ 取りに行った ─ 取得（'Attempt'）が1つ以上 ─ 取得の結果（'Outcome'）
--                                                                              └ 取れたページから読めたもの（'Listed'・項目の 'Field'）
-- @
--
-- 検索の状態（'Status'）は、取得の並びから計算する（'statusOf'）。持ち回らない。

-- | 取りに行ったページの役割
data Role
  = -- | 検索結果の1ページ目
    SearchPage
  | -- | 続きのページ（価格.com のショップ一覧など）
    FollowPage
  | -- | 語を広げた検索。広げた語と、その結果から残すものが題に含むべき語
    WiderPage Text [Text]
  | -- | 個別ページ
    DetailPage
  | -- | 送料の問い合わせ
    ShipQuery
  deriving stock (Eq, Show)

rolePairs :: Role -> [Pair]
rolePairs = \case
  SearchPage -> ["role" .= ("search" :: Text)]
  FollowPage -> ["role" .= ("follow" :: Text)]
  WiderPage w has -> ["role" .= ("wider" :: Text), "wider" .= object ["term" .= w, "has" .= has]]
  DetailPage -> ["role" .= ("item" :: Text)]
  ShipQuery -> ["role" .= ("shipping" :: Text)]

-- | 1ページを取りに行った結果
data Outcome
  = -- | 読めるページが返った
    Got
  | -- | HTTP 404
    NotFound
  | -- | ブロックされた（403・429・ボット確認・Access Denied）
    Blocked Block
  | -- | 取りに行かなかった。同じホストが、この実行でもうブロックされていた（そのブロック）
    HostBlocked Block
  | -- | メンテナンスのページ
    Stopped Text
  | -- | つながらない・応答がない・HTTP 500 など
    Failed Text
  | -- | 時間の上限までに終わらなかった
    TimedOut
  deriving stock (Eq, Show)

outcomePairs :: Outcome -> [Pair]
outcomePairs = \case
  Got -> ["outcome" .= ("got" :: Text)]
  NotFound -> ["outcome" .= ("not_found" :: Text)]
  Blocked b -> ["outcome" .= ("blocked" :: Text), "block" .= b]
  HostBlocked b -> ["outcome" .= ("host_blocked" :: Text), "block" .= b]
  Stopped why -> ["outcome" .= ("stopped" :: Text), "reason" .= why]
  Failed why -> ["outcome" .= ("failed" :: Text), "reason" .= why]
  TimedOut -> ["outcome" .= ("timed_out" :: Text)]

-- | ブロックされた事実：どのページで、何を見てブロックと決めたか。応答の手がかりも添える
data Block = Block
  { -- | ブロックされたページ
    url :: Text
  , -- | HTTP の状態コード
    status :: Int
  , -- | ブロックと決めた根拠（「HTTP 403」、「ページに「Access Denied」」など）
    reason :: Text
  , -- | ページの題
    title :: Maybe Text
  , -- | 応答の Server ヘッダ。どこの仕組みが止めたかの手がかり（AkamaiGHost・cloudflare など）
    server :: Maybe Text
  , -- | 応答の Retry-After ヘッダ（429 のとき、いつまで待てばよいか）
    retryAfter :: Maybe Text
  , -- | ページを取った時刻（JST）
    at :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 取れた一覧のページから読めたもの
data Listed = Listed
  { -- | 抜き出せた売り物の数
    offers :: Int
  , -- | サイトが「該当なし」と返した
    zero :: Bool
  , -- | そのうち、結果に残した数。URLの形が正しくないもの、もう出ているもの、
    -- 語を広げた検索で題にもとの語を含まないものは、残さない
    kept :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | 取得1回：どのページを、何のために取りに行き、どうなったか
data Attempt = Attempt
  { role :: Role
  , -- | 取りに行ったページ。配送先を含む問い合わせは、? より前だけ
    url :: Text
  , -- | 取った時刻（JST）。取れなかったときは、あきらめた時刻
    at :: Text
  , outcome :: Outcome
  , -- | 一覧のページが取れたとき、そこから読めたもの
    listed :: Maybe Listed
  }
  deriving stock (Eq, Show)

instance ToJSON Attempt where
  toJSON a = object (rolePairs a.role <> ["url" .= a.url, "at" .= a.at] <> outcomePairs a.outcome <> ["listed" .= l | Just l <- [a.listed]])

-- | うまくいった取得か。404 は、サイトが「該当なし」と返したときだけ（0件のページを 404 で返すサイトがある）
fine :: Attempt -> Bool
fine a = case a.outcome of
  Got -> True
  NotFound -> maybe False (.zero) a.listed
  _ -> False

-- | うまくいかなかった取得
troubles :: [Attempt] -> [Attempt]
troubles = filter (not . fine)

-- | うまくいかなかった取得の種類：blocked・stopped・timed_out・failed
failureKind :: Attempt -> Text
failureKind a = case a.outcome of
  Blocked _ -> "blocked"
  HostBlocked _ -> "blocked"
  Stopped _ -> "stopped"
  TimedOut -> "timed_out"
  _ -> "failed"

-- | 取りに行かなかった理由。取りに行く前に、カタログやURLの形で決まる
data Skip
  = -- | 買う販路ではない、新品・中古の別が合わない、など
    OutOfScope Text
  | -- | HTTP では読めない。ブラウザで開く
    NeedsBrowser Text
  | -- | カタログの記録で、どの方法でも読めなかった販路（確かめた日と注記）
    BlockedBefore Text Text
  | -- | カタログの記録で、止まっている販路（確かめた日と理由）
    StoppedBefore Text Text
  deriving stock (Eq, Show)

-- | 取りに行かなかった理由の種類：out_of_scope・needs_browser・blocked_before・stopped
skipKind :: Skip -> Text
skipKind = \case
  OutOfScope _ -> "out_of_scope"
  NeedsBrowser _ -> "needs_browser"
  BlockedBefore _ _ -> "blocked_before"
  StoppedBefore _ _ -> "stopped"

instance ToJSON Skip where
  toJSON s = object $ ("why" .= skipKind s) : case s of
    OutOfScope why -> ["reason" .= why]
    NeedsBrowser why -> ["reason" .= why]
    BlockedBefore day why -> ["checked" .= day, "reason" .= why]
    StoppedBefore day why -> ["checked" .= day, "reason" .= why]

-- | 検索1回や個別ページ1つについて、したこと
data Did
  = -- | 取りに行かなかった
    Skipped Skip
  | -- | 取りに行った。取得は、した順
    Tried [Attempt]
  deriving stock (Eq, Show)

attemptsOf :: Did -> [Attempt]
attemptsOf = \case
  Skipped _ -> []
  Tried as -> as

-- | JSON では、skipped（取りに行かなかった理由。取りに行ったなら null）と attempts（取得の並び）に分ける
didPairs :: Did -> [Pair]
didPairs d = case d of
  Skipped s -> ["skipped" .= s, "attempts" .= ([] :: [Attempt])]
  Tried as -> ["skipped" .= (Nothing :: Maybe Skip), "attempts" .= as]

instance ToJSON Did where
  toJSON = object . didPairs

-- | 「どこまで探したか」の1行：検索1回（1つの販路に、1つの語を、新品か中古のどちらかで当てる）
data Coverage = Coverage
  { site :: Text
  , term :: Text
  , market :: Market
  , -- | 検索URL。組み立てられないサイトは 'Nothing'
    url :: Maybe Text
  , did :: Did
  , -- | 残した売り物の最安（税込価格）
    cheapest :: Maybe Yen
  , -- | 見た方法（HTTP・カタログ・ブラウザで開く など）
    method :: Text
  , -- | 最初のページを取った時刻。取りに行かなかったなら、この行を作った時刻（JST）
    at :: Text
  }
  deriving stock (Eq, Show)

-- | JSON には、計算した状態（status と、残した数 found）も入れる
instance ToJSON Coverage where
  toJSON c =
    object $
      [ "site" .= c.site
      , "term" .= c.term
      , "market" .= c.market
      , "url" .= c.url
      , "status" .= statusKind st
      , "found" .= (case st of Found n _ -> n; _ -> 0)
      , "cheapest" .= c.cheapest
      ]
        <> didPairs c.did
        <> ["method" .= c.method, "at" .= c.at]
   where
    st = statusOf c

-- | 検索1回の状態
data Status
  = -- | 残した売り物の数と、最安（税込価格）
    Found Int (Maybe Yen)
  | -- | サイトが「該当なし」と返した
    Zero
  | -- | 取りに行ったが、1件も得られなかった。そのもとになった取得
    Unfetched Attempt
  | -- | ページは取れたが、一覧から1件も抜き出せなかった
    Unreadable
  | -- | 取りに行かなかった
    NotTried Skip
  deriving stock (Eq, Show)

-- | 検索の状態を、取得の並びから計算する。
--
-- 1件でも残れば「取得」。残らず、1ページ目が「該当なし」なら「0件」。
-- どちらでもなければ、うまくいかなかった最初の取得がもと。それも無ければ、抜き出せなかった。
--
-- 「取得」や「0件」でも、うまくいかなかった取得（続きのページ・語を広げた検索）が残っていることがある（'troubles'）。
statusOf :: Coverage -> Status
statusOf c = case c.did of
  Skipped s -> NotTried s
  Tried as ->
    let kept = sum [l.kept | a <- as, Just l <- [a.listed]]
        zero = or [l.zero | a <- as, a.role == SearchPage, Just l <- [a.listed]]
     in if
          | kept > 0 -> Found kept c.cheapest
          | zero -> Zero
          | Just a <- head (troubles as) -> Unfetched a
          | otherwise -> Unreadable

-- | 状態の種類：found・zero・blocked・stopped・timed_out・failed・out_of_scope・needs_browser・blocked_before
statusKind :: Status -> Text
statusKind = \case
  Found _ _ -> "found"
  Zero -> "zero"
  Unfetched a -> failureKind a
  Unreadable -> "failed"
  NotTried s -> skipKind s
