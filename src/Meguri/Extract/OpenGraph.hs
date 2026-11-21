-- | OGP（og:image・product:price:amount など）
module Meguri.Extract.OpenGraph (ogImage, ogTitle, ogPriceAmount) where

import Meguri.Extract.Html
import Meguri.Extract.Price (parseYen)
import Meguri.Types (Yen)
import Protolude

ogImage :: Tags -> Maybe Text
ogImage = metaContent "og:image"

ogTitle :: Tags -> Maybe Text
ogTitle = metaContent "og:title"

ogPriceAmount :: Tags -> Maybe Yen
ogPriceAmount ts = parseYen =<< (metaContent "product:price:amount" ts <|> metaContent "og:price:amount" ts)
