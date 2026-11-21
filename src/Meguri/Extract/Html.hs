-- | HTML を読むための部品（tagsoup）。すべて純粋。
module Meguri.Extract.Html
  ( Tags
  , parse
  , attr
  , tagOpenParts
  , hasClass
  , hasClassPrefix
  , classes
  , subtree
  , subtrees
  , elementsWhere
  , innerText
  , firstTextWhere
  , textOfClass
  , textOfClassPrefix
  , scriptsOfType
  , scriptById
  , allScripts
  , metaContent
  , linkHref
  , titleText
  , absUrl
  , scanJsonString
  , scanJsonNumber
  , scanJsonBool
  , unescapeJs
  ) where

import Data.Text qualified as T
import Meguri.Text (squish)
import Protolude
import Text.HTML.TagSoup (Tag (..), fromAttrib, innerText, isTagOpen, parseTags)

type Tags = [Tag Text]

parse :: Text -> Tags
parse = parseTags

attr :: Text -> Tag Text -> Maybe Text
attr k t@(TagOpen _ _) = case fromAttrib k t of
  "" -> Nothing
  v -> Just v
attr _ _ = Nothing

-- | 開始タグなら、名前と属性
tagOpenParts :: Tag Text -> Maybe (Text, [(Text, Text)])
tagOpenParts = \case
  TagOpen n as -> Just (n, as)
  _ -> Nothing

classes :: Tag Text -> [Text]
classes t = maybe [] T.words (attr "class" t)

hasClass :: Text -> Tag Text -> Bool
hasClass c t = c `elem` classes t

-- | ハッシュつきのクラス名（@price--3zUvK@）を前半で照らす
hasClassPrefix :: Text -> Tag Text -> Bool
hasClassPrefix p t = any (p `T.isPrefixOf`) (classes t)

voidTags :: [Text]
voidTags = ["br", "img", "input", "meta", "link", "hr", "wbr", "source", "area", "base", "col", "embed", "param", "track"]

-- | 先頭の開始タグに対応する終了タグまで（先頭が開始タグでなければ空）
subtree :: Tags -> Tags
subtree (o@(TagOpen n _) : rest)
  | n `elem` voidTags = [o]
  | otherwise = o : go (1 :: Int) rest
 where
  go _ [] = []
  go d (t : ts) = case t of
    TagOpen m _ | m == n -> t : go (d + 1) ts
    TagClose m | m == n -> if d == 1 then [t] else t : go (d - 1) ts
    _ -> t : go d ts
subtree _ = []

-- | 条件に合う要素それぞれの部分木
subtrees :: (Tag Text -> Bool) -> Tags -> [Tags]
subtrees p = go
 where
  go [] = []
  go ts@(t : rest)
    | isTagOpen t && p t = subtree ts : go rest
    | otherwise = go rest

elementsWhere :: (Tag Text -> Bool) -> Tags -> [Tag Text]
elementsWhere p = filter (\t -> isTagOpen t && p t)

firstTextWhere :: (Tag Text -> Bool) -> Tags -> Maybe Text
firstTextWhere p ts = case subtrees p ts of
  (s : _) -> nonEmpty' (squish (innerText s))
  [] -> Nothing

textOfClass :: Text -> Tags -> Maybe Text
textOfClass c = firstTextWhere (hasClass c)

textOfClassPrefix :: Text -> Tags -> Maybe Text
textOfClassPrefix c = firstTextWhere (hasClassPrefix c)

nonEmpty' :: Text -> Maybe Text
nonEmpty' t = if T.null t then Nothing else Just t

scriptBodies :: (Tag Text -> Bool) -> Tags -> [Text]
scriptBodies p = go
 where
  go (o@(TagOpen "script" _) : rest)
    | p o =
        let (body, rest') = break isScriptClose rest
         in T.concat [x | TagText x <- body] : go rest'
  go (_ : rest) = go rest
  go [] = []
  isScriptClose (TagClose "script") = True
  isScriptClose _ = False

scriptsOfType :: Text -> Tags -> [Text]
scriptsOfType ty = scriptBodies (\t -> (T.toLower <$> attr "type" t) == Just ty)

scriptById :: Text -> Tags -> Maybe Text
scriptById i = head . scriptBodies (\t -> attr "id" t == Just i)

allScripts :: Tags -> [Text]
allScripts = scriptBodies (const True)

-- | @<meta property|name|itemprop=key content=...>@
metaContent :: Text -> Tags -> Maybe Text
metaContent key ts =
  head
    [ c
    | t@(TagOpen "meta" _) <- ts
    , any (\k -> attr k t == Just key) ["property", "name", "itemprop"]
    , Just c <- [attr "content" t]
    ]

linkHref :: Text -> Tags -> Maybe Text
linkHref rel ts = head [h | t@(TagOpen "link" _) <- ts, attr "rel" t == Just rel, Just h <- [attr "href" t]]

titleText :: Tags -> Maybe Text
titleText ts = case dropWhile (/= TagOpen "title" []) ts of
  (_ : TagText t : _) -> nonEmpty' (squish t)
  _ -> Nothing

-- | 相対URLを絶対URLにする（@base@ は https://host の形）
absUrl :: Text -> Text -> Text
absUrl base u
  | "http://" `T.isPrefixOf` u || "https://" `T.isPrefixOf` u = u
  | "//" `T.isPrefixOf` u = "https:" <> u
  | "/" `T.isPrefixOf` u = origin <> u
  | otherwise = origin <> "/" <> u
 where
  origin = case T.splitOn "/" base of
    (scheme : "" : host : _) -> scheme <> "//" <> host
    _ -> base

-- | 埋め込みの JSON や JS から @"key": "value"@ の値を拾う（最初の1つ）
scanJsonString :: Text -> Text -> Maybe Text
scanJsonString key src = do
  rest <- afterKey key src
  body <- T.stripPrefix "\"" rest
  pure (unescapeJs (takeString body))
 where
  takeString t = case T.breakOn "\"" t of
    (a, b)
      | "\\" `T.isSuffixOf` a && not (T.null b) -> a <> "\"" <> takeString (T.drop 1 b)
      | otherwise -> a

scanJsonNumber :: Text -> Text -> Maybe Int
scanJsonNumber key src = do
  rest <- afterKey key src
  let ds = T.takeWhile isDigit (T.dropWhile (== '"') rest)
  if T.null ds then Nothing else readMaybe ds

scanJsonBool :: Text -> Text -> Maybe Bool
scanJsonBool key src = do
  rest <- afterKey key src
  if "true" `T.isPrefixOf` rest
    then Just True
    else if "false" `T.isPrefixOf` rest then Just False else Nothing

-- | @"key"@ の後ろの @:@ を越えた位置
afterKey :: Text -> Text -> Maybe Text
afterKey key src =
  let needle = "\"" <> key <> "\""
      (_, hit) = T.breakOn needle src
   in if T.null hit
        then Nothing
        else T.stripStart <$> T.stripPrefix ":" (T.stripStart (T.drop (T.length needle) hit))

unescapeJs :: Text -> Text
unescapeJs = T.replace "\\/" "/" . T.replace "\\\"" "\""
