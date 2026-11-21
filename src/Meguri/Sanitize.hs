-- | 出力に載せる文字列を整える（純粋）。
--
-- 商品名・店名などはページの書き手が自由に書ける文字列で、読むのはエージェント。
-- 長さを切り、制御文字を除く。URL は http(s) だけを通す。説明文は初めから抜き出さない。
module Meguri.Sanitize
  ( tidyOffer
  , usableUrl
  , publicUrl
  ) where

import Data.Text qualified as T
import Meguri.Text (clean, decodeEntities)
import Meguri.Types
import Protolude

-- | http(s) で、空白や制御文字を含まないURLだけを通す
usableUrl :: Text -> Bool
usableUrl u =
  (T.isPrefixOf "https://" u || T.isPrefixOf "http://" u)
    && not (T.any (\c -> isControl c || isSpace c) u)
    && T.length u <= 2000

-- | 公開のホストのURLか。ページに書かれたURLをたどって、手元や内部のホストを読みに行かない。
-- （localhost、IPアドレス、点の無い名前、.local などを通さない）
publicUrl :: Text -> Bool
publicUrl u = usableUrl u && T.any (== '.') host && not ipLike && not (any (`T.isSuffixOf` host) [".local", ".localhost", ".internal", ".lan", ".home.arpa"])
 where
  authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') (T.drop 3 (snd (T.breakOn "://" u)))
  -- user:pass@ を落とす
  hostPort = T.takeWhileEnd (/= '@') authority
  host = T.toLower (if "[" `T.isPrefixOf` hostPort then hostPort else T.takeWhile (/= ':') hostPort)
  ipLike = T.all (\c -> c `elem` ("0123456789." :: [Char])) host || "[" `T.isPrefixOf` host

tidyOffer :: Offer s -> Offer s
tidyOffer o =
  o
    { name = text 200 o.name
    , priceIncl = meta o.priceIncl
    , priceExcl = meta o.priceExcl
    , shipping = meta (fmap tidyShipping o.shipping)
    , stock = meta o.stock
    , seller = text 80 o.seller
    , image = url o.image
    , buy = meta (fmap tidyBuy o.buy)
    , points = meta o.points
    , condition = text 60 o.condition
    , keys =
        Keys
          { jan = clean 20 <$> o.keys.jan
          , model = clean 80 <$> o.keys.model
          , siteItemId = clean 80 <$> o.keys.siteItemId
          , sharedStockId = clean 20 <$> o.keys.sharedStockId
          }
    }
 where
  -- 注記と、もとの表示（raw）も切る
  meta :: Field s a -> Field s a
  meta f = f {raw = clean 120 <$> f.raw, note = clean 200 <$> f.note}
  text :: Int -> Field s Text -> Field s Text
  text n f = meta (fmap (clean n . decodeEntities) f)
  url :: Field s Text -> Field s Text
  -- JSON-LD の中のURLには、&amp; が入っていることがある
  url f0 =
    let f = fmap decodeEntities f0
     in case f.value of
          Just u | not (usableUrl u) -> (meta f) {value = Nothing, note = Just "URLの形が正しくない"}
          _ -> meta f
  tidyShipping = \case
    ShipUnknown t -> ShipUnknown (clean 120 t)
    other -> other
  tidyBuy = \case
    BuyActive t -> BuyActive (clean 40 t)
    BuyDisabled t -> BuyDisabled (clean 40 t)
