-- | アダプタの共通の形。アダプタは「HTMLの文字列 → 値」の純粋な関数。
module Meguri.Site.Common
  ( ListCtx (..)
  , ListOut (..)
  , ItemCtx (..)
  , Adapter (..)
  , Draft
  , noAdapter
  , listOffer
  , found
  , sl
  , slMaybe
  , bodyField
  , kvPairs
  , stripQuery
  , thirteenDigits
  , textHas
  ) where

import Data.Text qualified as T
import Meguri.Extract.Generic (Draft)
import Meguri.Extract.Html (Tags)
import Meguri.Types
import Protolude

data ListCtx = ListCtx
  { siteId :: Text
  , term :: Text
  , market :: Market
  , pageUrl :: Text
  , httpStatus :: Int
  , stamp :: Text
  }

data ListOut = ListOut
  { offers :: [Offer OnList]
  , -- | 本当の0件（サイトが「該当なし」と返した）
    zero :: Bool
  , -- | 続けて取るページ（価格.com のショップ一覧など）
    follow :: [Text]
  }

data ItemCtx = ItemCtx
  { pageUrl :: Text
  , tags :: Tags
  , html :: Text
  }

data Adapter = Adapter
  { listX :: Maybe (ListCtx -> Tags -> Text -> ListOut)
  , -- | 汎用の抽出の結果に、サイトの癖を上書きする
    itemX :: Maybe (ItemCtx -> Draft -> Draft)
  }

noAdapter :: Adapter
noAdapter = Adapter Nothing Nothing

listOffer :: ListCtx -> Text -> Offer OnList
listOffer ctx url = emptyOffer SearchList ctx.siteId url ctx.stamp

found :: [Offer OnList] -> ListOut
found os = ListOut os False []

sl :: a -> Field OnList a
sl = got SearchList

-- | 値があれば入れ、無ければ理由を書く
slMaybe :: Text -> Maybe a -> Field OnList a
slMaybe why = maybe (missing SearchList why) sl

bodyField :: Method -> Text -> Maybe a -> Field Method a
bodyField m why = maybe (missing m why) (got m)

-- | @k:v;k:v@ の形（Yahoo! の data-beacon）
kvPairs :: Text -> [(Text, Text)]
kvPairs t = [(k, T.drop 1 v) | p <- T.splitOn ";" t, let (k, v) = T.breakOn ":" p, not (T.null v)]

stripQuery :: Text -> Text
stripQuery = T.takeWhile (\c -> c /= '?' && c /= '#')

-- | URL の中の13桁の番号（2nd STREET の商品番号）
thirteenDigits :: Text -> Maybe Text
thirteenDigits u = find (\s -> T.length s == 13 && T.all isDigit s) (T.split (not . isDigit) u)

textHas :: [Text] -> Text -> Bool
textHas ks t = any (`T.isInfixOf` t) ks
