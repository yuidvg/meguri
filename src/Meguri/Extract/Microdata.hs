-- | microdata（itemprop）。Product の範囲の中だけを読む。
--
-- ページには、パンくずリストや店の情報にも itemprop="name" がある。
-- 範囲を限らないと、それを商品名として拾ってしまう。
module Meguri.Extract.Microdata
  ( itemprop
  , itemprops
  , productScope
  ) where

import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Text (squish)
import Protolude
import Text.HTML.TagSoup (Tag (..), isTagOpen)

-- | @itemtype@ が schema.org/Product の要素の部分木（最初の1つ）
productScope :: Tags -> Maybe Tags
productScope ts =
  head
    [ subtree rest
    | rest@(t : _) <- tails ts
    , isTagOpen t
    , maybe False (\ty -> any (`T.isSuffixOf` ty) ["schema.org/Product", "schema.org/IndividualProduct"]) (attr "itemtype" t)
    ]

hasAttr :: Text -> Tag Text -> Bool
hasAttr k = \case
  TagOpen _ as -> any ((== k) . fst) as
  _ -> False

-- | Product の範囲にある itemprop の値。content 属性があればそれ、無ければ要素の文字。
-- 入れ子になった別の itemscope（パンくず・レビューなど）は読まない。Offer の中は読む。
itemprops :: Text -> Tags -> [Text]
itemprops key ts = case productScope ts of
  Just (_ : inner) -> go inner
  _ -> []
 where
  go [] = []
  go here@(t : rest)
    | isTagOpen t && hasAttr "itemscope" t && not (isOffer t) = go (drop (length (subtree here)) here)
    | isTagOpen t && attr "itemprop" t == Just key =
        let v = attr "content" t <|> attr "href" t <|> attr "src" t <|> nonEmptyText (squish (innerText (subtree here)))
         in maybe identity (:) v (go rest)
    | otherwise = go rest
  isOffer t = maybe False (\ty -> "Offer" `T.isSuffixOf` ty) (attr "itemtype" t) || attr "itemprop" t == Just "offers"
  nonEmptyText x = if T.null x then Nothing else Just x

itemprop :: Text -> Tags -> Maybe Text
itemprop key = head . itemprops key
