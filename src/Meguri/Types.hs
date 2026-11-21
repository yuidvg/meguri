{-# LANGUAGE DeriveAnyClass #-}

-- | 値の型。値ごとに出どころ（'Field'）を持たせる。
--
-- 確認済みの値は、個別ページからの抽出でしか作れない。
-- 一覧やAPIの値は @Offer ListSource@、個別ページの値は @Offer ItemSource@ で、
-- 確認済みの判定（"Meguri.Confirm"）は後者しか受け取らない。
module Meguri.Types
  ( Yen (..)
  , unYen
  , Shipping (..)
  , shippingCost
  , Stock (..)
  , BuyEntry (..)
  , Method (..)
  , ItemSource (..)
  , ListSource (..)
  , PageOrigin (..)
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
  , Candidate (..)
  , ItemRecord (..)
  , Kind (..)
  , Status (..)
  , Coverage (..)
  , Confirmation (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
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

-- | 個別ページだけが持てる出どころ
data ItemSource
  = -- | HTTP で取った個別ページ
    ItemPage Method
  | -- | 通常のブラウザで保存した個別ページ
    Captured Method
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 一覧・APIの出どころ（確認済みにはならない）
data ListSource = SearchList | Api Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PageOrigin = FromHttp | FromCapture
  deriving stock (Eq, Show)

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

data Kind = New | Used
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 一覧やAPIから作った候補（未確認）
data Candidate = Candidate
  { query :: Text
  , kind :: Kind
  , offer :: Offer ListSource
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 個別ページから作った記録
data ItemRecord = ItemRecord
  { offer :: Offer ItemSource
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

data Status
  = -- | 件数と最安（税込価格）
    Found Int (Maybe Yen)
  | Zero
  | Blocked Text
  | NeedsBrowser Text
  | -- | 日付と注記
    Stopped Text Text
  | OutOfScope Text
  | Error Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 「どこまで探したか」の1行
data Coverage = Coverage
  { site :: Text
  , query :: Text
  , kind :: Kind
  , status :: Status
  , url :: Maybe Text
  , method :: Text
  , at :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
