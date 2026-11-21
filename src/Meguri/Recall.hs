{-# LANGUAGE DeriveAnyClass #-}

-- | recall：消費者庁と CPSC のリコール情報（一覧）。抽出は純粋。
module Meguri.Recall
  ( Recall (..)
  , caaRequest
  , cpscUrl
  , parseCaa
  , parseCpsc
  , recallLine
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text qualified as T
import Meguri.Extract.Html
import Meguri.Extract.JsonLd (arr, at, decodeLenient, int, str)
import Meguri.Fetch.Http (Req (..))
import Meguri.Text (clean, decodeEntities, percentEncode)
import Protolude

data Recall = Recall
  { source :: Text
  , recallId :: Text
  , title :: Text
  , -- | 掲載日（消費者庁）・リコールの日付（CPSC）
    date :: Maybe Text
  , url :: Text
  , -- | 分類・対象の製品・危険の内容など（短く切ったもの）
    detail :: Maybe Text
  , query :: Text
  , at :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 消費者庁リコール情報サイト。検索は読み取り専用の POST。
caaRequest :: Text -> Req
caaRequest q = Req "https://www.recall.caa.go.jp/result/index.php" (Just [("search", q), ("screenkbn", "01"), ("category", "0")]) False

-- | CPSC の公式API
cpscUrl :: Text -> Text
cpscUrl q = "https://www.saferproducts.gov/RestWebServices/Recall?format=json&ProductName=" <> percentEncode (encodeUtf8 q)

-- | 消費者庁の検索結果（表の1行が1件）
parseCaa :: Text -> Text -> Tags -> [Recall]
parseCaa query stamp ts = mapMaybe row (subtrees isRow ts)
 where
  isRow t = fmap fst (tagOpenParts t) == Just "tr"
  row el = do
    link <- find (maybe False ("detail.php?rcl=" `T.isInfixOf`) . attr "href") el
    href <- attr "href" link
    let rid = T.takeWhile (`elem` ("0123456789" :: [Char])) (T.drop (T.length "rcl=") (snd (T.breakOn "rcl=" href)))
        text = firstTextWhere (maybe False ("detail.php?rcl=" `T.isInfixOf`) . attr "href") el
    guard (not (T.null rid))
    pure
      Recall
        { source = "消費者庁"
        , recallId = rid
        , title = clean 120 (decodeEntities (fromMaybe "" text))
        , date = clean 12 <$> textOfClass "result_list_post_date" el
        , url = "https://www.recall.caa.go.jp/result/detail.php?rcl=" <> rid <> "&screenkbn=01"
        , detail = clean 40 <$> firstTextWhere (hasClass "result_list_category") el
        , query
        , at = stamp
        }

-- | CPSC の応答（JSON の配列）
parseCpsc :: Text -> Text -> Text -> [Recall]
parseCpsc query stamp body = maybe [] (mapMaybe one . arr) (decodeLenient body :: Maybe Value)
 where
  one v = do
    u <- str =<< at "URL" v
    t <- str =<< at "Title" v
    let names k field = T.intercalate "／" (take 3 (mapMaybe (str <=< at field) (maybe [] arr (at k v))))
        products = names "Products" "Name"
        hazards = names "Hazards" "Name"
    pure
      Recall
        { source = "CPSC"
        , recallId = fromMaybe "" ((str =<< at "RecallNumber" v) <|> (show <$> (int =<< at "RecallID" v)))
        , title = clean 120 t
        , date = T.take 10 <$> (str =<< at "RecallDate" v)
        , url = clean 300 u
        , detail = Just (clean 200 (T.intercalate "｜" (filter (not . T.null) [products, hazards])))
        , query
        , at = stamp
        }

-- | 1件1行：出どころ・日付・題・URL
recallLine :: Recall -> Text
recallLine r = T.intercalate "\t" [r.source, fromMaybe "-" r.date, clean 70 r.title, maybe "" (clean 60) r.detail, clean 200 r.url]
