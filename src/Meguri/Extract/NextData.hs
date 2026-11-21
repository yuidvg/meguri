-- | Next.js の __NEXT_DATA__
module Meguri.Extract.NextData (nextData, pageProps) where

import Data.Aeson (Value)
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (atPath, decodeLenient)
import Protolude

nextData :: Tags -> Maybe Value
nextData ts = decodeLenient =<< scriptById "__NEXT_DATA__" ts

pageProps :: Tags -> Maybe Value
pageProps ts = atPath ["props", "pageProps"] =<< nextData ts
