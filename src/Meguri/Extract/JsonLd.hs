-- | JSON-LD（@graph・hasVariant・AggregateOffer も扱う）
module Meguri.Extract.JsonLd
  ( LdProduct (..)
  , LdOffer (..)
  , ldValues
  , ldProducts
  , ldProduct
  , decodeLenient
  , str
  , int
  , at
  , atPath
  , arr
  ) where

import Data.Aeson (Value (..), decodeStrict')
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Vector qualified as V
import Meguri.Extract.Html (Tags, scriptsOfType)
import Meguri.Extract.Price (parseYen)
import Meguri.Types (unYen)
import Protolude

data LdOffer = LdOffer
  { price :: Maybe Int
  , availability :: Maybe Text
  , sellerName :: Maybe Text
  , itemCondition :: Maybe Text
  , shippingValue :: Maybe Int
  , offerUrl :: Maybe Text
  }
  deriving stock (Eq, Show)

data LdProduct = LdProduct
  { name :: Maybe Text
  , images :: [Text]
  , sku :: Maybe Text
  , gtin :: Maybe Text
  , mpn :: Maybe Text
  , brand :: Maybe Text
  , offers :: [LdOffer]
  }
  deriving stock (Eq, Show)

-- | 文字列の中に生の改行などがある JSON も読む
decodeLenient :: Text -> Maybe Value
decodeLenient = decodeStrict' . encodeUtf8 . T.map (\c -> if isControl c then ' ' else c)

-- | ページ内の JSON-LD を、配列と @graph を開いて並べる
ldValues :: Tags -> [Value]
ldValues ts = concatMap flatten (mapMaybe decodeLenient (scriptsOfType "application/ld+json" ts))
 where
  flatten v = case v of
    Array xs -> concatMap flatten (V.toList xs)
    Object o | Just g <- KM.lookup "@graph" o -> flatten g
    _ -> [v]

isType :: [Text] -> Value -> Bool
isType names v = case at "@type" v of
  Just (String s) -> matches s
  Just (Array xs) -> any (\x -> case x of String s -> matches s; _ -> False) (V.toList xs)
  _ -> False
 where
  -- 大文字小文字を区別しない（"product" と書くサイトがある）
  matches s = T.toLower s `elem` map T.toLower names

ldProducts :: [Value] -> [LdProduct]
ldProducts = map toProduct . filter (isType ["Product", "ProductGroup", "IndividualProduct"])

-- | 最初の Product
ldProduct :: Tags -> Maybe LdProduct
ldProduct = head . ldProducts . ldValues

toProduct :: Value -> LdProduct
toProduct v =
  LdProduct
    { name = str =<< at "name" v
    , images = maybe [] imageUrls (at "image" v)
    , sku = text' =<< at "sku" v
    , gtin = nonBlank =<< (text' =<< (at "gtin13" v <|> at "gtin" v <|> at "gtin12" v <|> at "gtin8" v))
    , mpn = text' =<< at "mpn" v
    , brand = (str =<< at "brand" v) <|> (str =<< atPath ["brand", "name"] v)
    , offers = own <> variants
    }
 where
  own = maybe [] toOffers (at "offers" v)
  variants = concat [maybe [] toOffers (at "offers" x) | x <- maybe [] arr (at "hasVariant" v)]
  nonBlank t = if T.null (T.strip t) then Nothing else Just t

imageUrls :: Value -> [Text]
imageUrls = \case
  String s -> [s]
  Array xs -> concatMap imageUrls (V.toList xs)
  o@(Object _) -> maybeToList (str =<< (at "url" o <|> at "contentUrl" o))
  _ -> []

toOffers :: Value -> [LdOffer]
toOffers = \case
  Array xs -> concatMap toOffers (V.toList xs)
  o@(Object _) ->
    let nested = maybe [] toOffers (at "offers" o)
        self =
          LdOffer
            { price =
                (int =<< at "price" o)
                  <|> (int =<< at "Price" o)
                  <|> (int =<< at "lowPrice" o)
                  <|> (int =<< atPath ["priceSpecification", "price"] o)
                  <|> (int =<< at "price" =<< head . arr =<< at "priceSpecification" o)
            , availability = str =<< at "availability" o
            , sellerName = (str =<< atPath ["seller", "name"] o) <|> (str =<< at "seller" o)
            , itemCondition = str =<< at "itemCondition" o
            , shippingValue =
                (int =<< atPath ["shippingDetails", "shippingRate", "value"] o)
                  <|> (int =<< atPath ["shippingRate", "value"] =<< head . arr =<< at "shippingDetails" o)
            , offerUrl = str =<< at "url" o
            }
     in if isJust self.price || null nested then self : nested else nested
  _ -> []

at :: Text -> Value -> Maybe Value
at k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

atPath :: [Text] -> Value -> Maybe Value
atPath ks v = foldM (flip at) v ks

arr :: Value -> [Value]
arr = \case
  Array xs -> V.toList xs
  Null -> []
  v -> [v]

str :: Value -> Maybe Text
str = \case
  String s | not (T.null (T.strip s)) -> Just (T.strip s)
  _ -> Nothing

-- | 文字列でも数でも文字列にする
text' :: Value -> Maybe Text
text' = \case
  String s | not (T.null (T.strip s)) -> Just (T.strip s)
  Number n -> show <$> (toBoundedInteger n :: Maybe Int)
  _ -> Nothing

-- | 数でも「1,980」の文字列でも整数にする（小数は切り捨てない：整数でなければ読まない）
int :: Value -> Maybe Int
int = \case
  Number n -> toBoundedInteger n <|> (let r = round n :: Int in if fromIntegral r == n then Just r else Nothing)
  String s ->
    let t = T.strip s
        whole = fromMaybe t (T.stripSuffix ".00" t <|> T.stripSuffix ".0" t)
     in if T.any (== '.') whole then Nothing else unYen <$> parseYen whole
  _ -> Nothing
