-- | 個別ページのURLの形との照合（カタログとアダプタの両方から使う）
module Meguri.Sites.Match (matchesItem) where

import Meguri.Catalog (SiteDef (..))
import Protolude
import Text.Regex.TDFA ((=~))

-- | そのサイトの個別ページのURLの形に合うか
matchesItem :: SiteDef -> Text -> Bool
matchesItem s u = any (\p -> (toS u :: [Char]) =~ (toS p :: [Char])) s.itemPatterns
