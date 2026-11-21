-- | 価格と送料の読み取り（純粋）
module Meguri.Extract.Price
  ( parseYen
  , TaxMark (..)
  , taxMark
  , parsePrice
  , parseShipping
  , stockFromSchema
  ) where

import Data.Text qualified as T
import Meguri.Text (toHalfWidth)
import Meguri.Types
import Protolude

-- | 文字列の最初の金額。「¥1,980（税込）」「1,980円」「１，９８０円」を読む。
parseYen :: Text -> Maybe Yen
parseYen t0 =
  let t = T.dropWhile (not . isDigit) (toHalfWidth t0)
      numeric = T.takeWhile (\c -> isDigit c || c == ',') t
      ds = T.filter isDigit numeric
   in if T.null ds || T.length ds > 9 then Nothing else Yen <$> readMaybe ds

data TaxMark = MarkIncl | MarkExcl
  deriving stock (Eq, Show)

-- | 税込・税抜の表示。無ければ 'Nothing'（決めつけない）。
taxMark :: Text -> Maybe TaxMark
taxMark t0
  | any (`T.isInfixOf` t) ["税抜", "税別", "本体価格", "+税", "税抜き"] = Just MarkExcl
  | any (`T.isInfixOf` t) ["税込", "(込)", "税込み"] = Just MarkIncl
  | otherwise = Nothing
 where
  t = toHalfWidth t0

-- | 金額と、税の表示
parsePrice :: Text -> Maybe (Yen, Maybe TaxMark)
parsePrice t = (,taxMark t) <$> parseYen t

-- | 送料の表示を読む。読めなければ 'Nothing'。
parseShipping :: Text -> Maybe Shipping
parseShipping t0
  | T.null t = Nothing
  | has ["円以上"] && has ["送料無料", "無料"] =
      -- 「送料550円（3,300円以上で送料無料）」
      let (before, after) = T.breakOn "円以上" t
          threshold = lastYen before
          fee = feeIn (T.replace (lastNumberText before <> "円以上") "" t) <|> feeIn after
       in case (threshold, fee) of
            (Just th, Just f) -> Just (FreeOver th f)
            _ -> Just (ShipUnknown (T.take 60 t))
  | has ["送料無料", "送料込", "送料:無料", "送料 無料", "送料出品者負担", "出品者負担", "無料配送", "配送料無料"] = Just Free
  | has ["着払", "落札者負担", "購入者負担"] = Just BuyerPays
  | has ["送料別", "配送先", "都道府県", "地域別", "地域により"] && isNothing (feeIn t) = Just ByDestination
  | Just f <- feeIn t = Just (if f == 0 then Free else Fixed f)
  | otherwise = Nothing
 where
  t = T.strip (toHalfWidth t0)
  has = any (`T.isInfixOf` t)
  -- 「送料」「配送料」の後ろの金額
  feeIn s = case filter (not . T.null . snd) [T.breakOn k s | k <- ["送料", "配送料", "配達料"]] of
    ((_, hit) : _) ->
      let rest = T.take 16 (T.drop 2 hit)
       in if "円" `T.isInfixOf` rest || "¥" `T.isInfixOf` rest then parseYen rest else Nothing
    [] -> Nothing
  lastNumberText s = T.takeWhileEnd (\c -> isDigit c || c == ',') s
  lastYen s = parseYen (lastNumberText s)

-- | schema.org の availability
stockFromSchema :: Text -> Maybe Stock
stockFromSchema u
  | any (`T.isSuffixOf` l) ["instock", "limitedavailability", "instoreonly", "onlineonly"] = Just InStock
  | any (`T.isSuffixOf` l) ["outofstock", "soldout", "discontinued"] = Just OutOfStock
  | any (`T.isSuffixOf` l) ["preorder", "backorder", "presale"] = Just Backorder
  | otherwise = Nothing
 where
  l = T.toLower (T.strip u)
