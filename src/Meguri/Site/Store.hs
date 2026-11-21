-- | 直販の店に共通の部品。
--
-- 多くの直販サイトは、名前・価格・在庫・画像を JSON-LD に持つ。足りないのは、
-- 販売者（直販なので店そのもの）、購入の入口、送料、価格が税込かどうかの裏づけ。
-- ここは、それを「ページに書いてあること」から読むための部品。
module Meguri.Site.Store
  ( visibleText
  , visibleTexts
  , compact
  , taxInclConfirmed
  , confirmPrice
  , freeOver
  , cartWhere
  , hasText
  , buttonWith
  , yenBefore
  , taxInclAmount
  , stockFromWords
  , stockField
  , validEan13
  , cards
  , CardOpts (..)
  , defaultCards
  ) where

import Data.List (nubBy)
import Data.Text qualified as T
import Meguri.Catalog (SiteDef (..))
import Meguri.Extract.Html
import Meguri.Extract.Price (parseShipping, parseYen)
import Meguri.Sanitize (usableUrl)
import Meguri.Site.Common
import Meguri.Sites.Match (matchesItem)
import Meguri.Text (decodeEntities, squish, toHalfWidth)
import Meguri.Types
import Protolude
import Text.HTML.TagSoup (Tag (..), isTagOpen)

-- | 画面に出る文字を、文字の要素ごとに（script・style の中身を除く）
visibleTexts :: Tags -> [Text]
visibleTexts = filter (not . T.null) . map squish . go
 where
  go (TagOpen n _ : rest) | n `elem` ["script", "style", "noscript", "template"] = go (drop 1 (dropWhile (/= TagClose n) rest))
  go (TagText t : rest) = t : go rest
  go (_ : rest) = go rest
  go [] = []

-- | 画面に出る文字を、1つにつないだもの
visibleText :: Tags -> Text
visibleText = T.unwords . visibleTexts

-- | 空白を除き、全角の英数字と記号を半角にした文字列（「37 ,980 円 （税込）」→「37,980円(税込)」）
compact :: Text -> Text
compact = T.filter (not . isSpace) . toHalfWidth

-- | 本文に、その額が「税込」と並んで書いてあるか。
-- 「¥13,800税込」「13,800円(税込)」「税込13,800円」のどれか。
taxInclConfirmed :: Text -> Yen -> Bool
taxInclConfirmed body (Yen n) = any after forms || any before forms
 where
  c = compact body
  plain = show n
  commas = T.intercalate "," (reverse (map T.reverse (T.chunksOf 3 (T.reverse plain))))
  forms = ordNub [commas, plain]
  numeric ch = isDigit ch || ch == ',' || ch == '.'
  -- 額の後ろに「税込」：「13,800税込」「13,800円(税込)」。額の前が数字の続きでないこと
  after p = any (\suffix -> occurs (p <> suffix) (\pre _ -> not (endsNumeric pre))) ["税込", "(税込)", "円税込", "円(税込)", "円税込み", "円(税込み)", "消費税込", "円消費税込", "価格¥" <> p <> "消費税込"]
  -- 額の前に「税込」：「税込13,800円」。額の後ろが数字の続きでないこと
  before p = any (\prefix -> occurs (prefix <> p) (\_ post -> not (startsNumeric post))) ["税込", "税込¥", "税込:", "(税込)", "(税込)¥", "税込価格", "税込価格:", "税込価格¥"]
  endsNumeric t = maybe False (numeric . snd) (T.unsnoc t)
  startsNumeric t = maybe False (numeric . fst) (T.uncons t)
  -- needle が現れるすべての場所で、前後の文字列を調べる
  occurs needle ok = any (\(pre, post) -> ok pre (T.drop (T.length needle) post)) (T.breakOnAll needle c)

-- | JSON-LD などの価格を、本文の税込の表示と照らす。合わなければ税込価格は null にする。
confirmPrice :: Text -> Field Method Yen -> Field Method Yen
confirmPrice body f = case f.value of
  Just y
    | taxInclConfirmed body y -> f {note = Just "本文の税込の表示と同じ額"}
    | otherwise -> (missing f.source "本文で、この額が税込だと確かめられなかった") {raw = Just (show (unYen y))}
  Nothing -> f

-- | 「◯円以上で送料無料」。この商品がその額以上なら無料、未満なら送料は分からない（ページに額が無い）。
freeOver :: Method -> Yen -> Text -> Maybe Yen -> Field Method Shipping
freeOver m threshold raw price = case price of
  Just p
    | p >= threshold -> (gotRaw m raw Free) {note = Just "合計がこの額以上なら送料無料（この商品だけで届く）。大型の商品などは別のことがある"}
  _ -> gotRaw m raw (ShipUnknown (raw <> "。未満の送料は、ご利用ガイドで確かめる"))

-- | 条件に合う要素があれば、購入の入口として読む。disabled が付いていれば無効。
cartWhere :: Text -> (Tag Text -> Bool) -> Tags -> Maybe BuyEntry
cartWhere label p ts = do
  t <- find (\x -> isTagOpen x && p x) ts
  pure (if disabled t then BuyDisabled label else BuyActive label)
 where
  disabled = \case
    TagOpen _ as -> any ((== "disabled") . fst) as
    _ -> False

hasText :: Text -> Tags -> (Tag Text -> Bool)
hasText needle ts t = case dropWhile (/= t) ts of
  (_ : rest) -> needle `T.isInfixOf` T.concat [x | TagText x <- takeWhile notClose (take 6 rest)]
  [] -> False
 where
  notClose = \case
    TagClose _ -> False
    _ -> True

-- | 文字を含む button 要素を、購入の入口として読む。disabled が付いていれば無効。
-- ボタンの中に style などの要素が挟まっていても読める。
buttonWith :: Text -> Tags -> Maybe BuyEntry
buttonWith label ts = do
  b <- find (\s -> label `T.isInfixOf` visibleText s) (subtrees isButton ts)
  t <- head b
  pure (if disabled t then BuyDisabled label else BuyActive label)
 where
  isButton = \case
    TagOpen "button" _ -> True
    _ -> False
  disabled = \case
    TagOpen _ as -> any ((== "disabled") . fst) as
    _ -> False

-- | 「61」「円」「（税込）」と、別々の要素に分かれて並んだ最初の金額。
-- 金額の要素は数字と桁区切りだけなので、手前の文字（ブランド名の数字など）を巻き込まない。
taxInclAmount :: [Text] -> Maybe Yen
taxInclAmount ts =
  head
    [ y
    | (a : b : c : _) <- tails (map compact ts)
    , b == "円"
    , "(税込)" `T.isPrefixOf` c
    , not (T.null a)
    , T.all (\ch -> isDigit ch || ch == ',') a
    , Just y <- [parseYen a]
    ]

-- | 在庫の表示（店ごとの言い回し）を読む
stockFromWords :: Text -> Maybe Stock
stockFromWords t
  | has ["在庫なし", "在庫切れ", "販売終了", "販売休止", "完売", "お取り扱いできません", "売り切れ"] = Just OutOfStock
  | has ["お取り寄せ", "取り寄せ", "予約", "入荷時期", "以内に発送", "入荷次第"] = Just Backorder
  | has ["在庫あり", "在庫残少", "在庫僅少", "在庫わずか", "残り", "お届け"] = Just InStock
  | otherwise = Nothing
 where
  has = any (`T.isInfixOf` t)

stockField :: Text -> Maybe Text -> Field Method Stock
stockField why = \case
  Just t | Just s <- stockFromWords t -> gotRaw Body t s
  Just t -> (missing Body "在庫の表示を解釈できなかった") {raw = Just t}
  Nothing -> missing Body why

-- | 13桁で、チェックディジットが合っている（JAN / EAN-13）
validEan13 :: Text -> Bool
validEan13 t =
  T.length t == 13
    && T.all isDigit t
    && case reverse (map digitToInt (T.unpack t)) of
      (check : rest) -> (10 - sum (zipWith (*) (reverse rest) (cycle [1, 3])) `mod` 10) `mod` 10 == check
      [] -> False

-- | 目印の文字の直前（か直後）にある金額。「基本配送料： ¥ 880」など。
yenBefore :: Text -> Text -> Maybe Yen
yenBefore marker body =
  let c = compact body
      (_, hit) = T.breakOn (compact marker) c
      after = T.take 14 (T.drop (T.length (compact marker)) hit)
   in if T.null hit then Nothing else parseYen (T.takeWhile (\ch -> isDigit ch || ch `elem` (",¥:：円" :: [Char])) after)

-- 一覧 ----------------------------------------------------------------------

data CardOpts = CardOpts
  { -- | 価格の要素のクラス（前半一致）。無ければ、カードの中の最初の金額
    priceClass :: Maybe Text
  , -- | 名前の要素のクラス（前半一致）。無ければ、画像の alt かリンクの文字
    nameClass :: Maybe Text
  , -- | 直販なら店の名前
    sellerName :: Maybe Text
  , -- | カードに出品者が出るサイトで、その要素のクラス（完全一致）
    sellerClass :: Maybe Text
  , -- | カードに送料が出るサイトで、その要素のクラス（完全一致）
    shippingClass :: Maybe Text
  , -- | 検索結果の入れ物の id。この中のリンクだけを読む（おすすめ・ランキングなどの枠の商品を拾わない）。
    -- 入れ物があって商品が無ければ、0件と読む
    regionId :: Maybe Text
  , -- | 0件のページにだけ出る文字
    zeroMarks :: [Text]
  , -- | この文字があれば、商品のカードが並んでいても0件と読む（該当なしのページにおすすめを並べるサイト）
    emptyMarks :: [Text]
  , -- | カードに出ていれば売り切れと読む文字。値札のひな形に「売り切れ」が入っているサイトでは空にする
    soldOutMarks :: [Text]
  , -- | 一覧の価格が税込か（サイトの決まり）
    taxIncluded :: Bool
  , -- | 個別ページのURLから、? より後ろ（検索語などの目印）を落とす
    dropQuery :: Bool
  }

defaultCards :: CardOpts
defaultCards =
  CardOpts
    { priceClass = Nothing
    , nameClass = Nothing
    , sellerName = Nothing
    , sellerClass = Nothing
    , shippingClass = Nothing
    , regionId = Nothing
    , zeroMarks = ["見つかりませんでした", "該当する商品がありません", "一致する商品はありません"]
    , emptyMarks = []
    , soldOutMarks = ["売り切れ", "在庫なし", "SOLD OUT", "品切れ", "販売終了"]
    , taxIncluded = True
    , dropQuery = False
    }

-- | 検索結果を「個別ページへのリンクごとのカード」に切って読む。
--
-- カードは、ある個別ページへの最初のリンクから、次の別の個別ページへの最初のリンクの手前まで。
-- サイトごとに fixture と golden で確かめてから使う。その店に無い語（0件）のページでも確かめる：
-- 該当なしのページや検索結果の後ろに、おすすめ・ランキング・最近売れた商品の枠を並べるサイトがある。
cards :: SiteDef -> CardOpts -> ListCtx -> Tags -> Text -> ListOut
cards def o ctx page html =
  let region = o.regionId >>= \i -> head (subtrees (\t -> attr "id" t == Just i) page)
      ts = case o.regionId of
        Just _ -> fromMaybe [] region
        Nothing -> page
      forcedZero = textHas o.emptyMarks html
      indexed = zip [0 :: Int ..] ts
      links =
        nubBy ((==) `on` snd) $
          [ (i, u)
          | (i, t) <- indexed
          , Just h <- [attr "href" t]
          , let u = stripTracking (absUrl ctx.pageUrl (decodeEntities h))
          , usableUrl u
          , matchesItem def u
          ]
      spans = zipWith (\(i, u) next -> (u, take (min 400 (next - i)) (drop i ts))) links (map fst (drop 1 links) <> [length ts])
      offers = if forcedZero then [] else mapMaybe one spans
   in ListOut {offers, zero = null offers && (forcedZero || isJust region || textHas o.zeroMarks html), follow = []}
 where
  stripTracking u = if o.dropQuery then stripQuery u else u
  one (u, card) =
    let texts = [squish x | TagText x <- card, not (T.null (T.strip x))]
        joined = T.unwords texts
        byClass c = firstTextWhere (hasClassPrefix c) card
        exact c = firstTextWhere (hasClass c) card
        alt = find (\a -> T.length a > 3) (mapMaybe (attr "alt") card)
        linkText = find (\x -> T.length x > 3) texts
        name = (o.nameClass >>= byClass) <|> alt <|> linkText
        priceText = (o.priceClass >>= byClass) <|> firstPrice joined
        -- 商品の画像は、alt に名前が入っているものを先に探す（評価の星などの飾りを拾わない）
        imgUrls is = filter usableUrl (map (absUrl ctx.pageUrl) (concatMap (\t -> mapMaybe (`attr` t) ["data-src", "data-original", "src"]) is))
        named = filter (\t -> maybe False ((> 3) . T.length) (attr "alt" t)) (filter isImg card)
        -- alt が商品名と重なる画像を、いちばん先に（SALE などの印の画像を拾わない）
        sameName = filter (\t -> maybe False (\a -> maybe False (\n -> T.take 6 n `T.isInfixOf` a || T.take 6 a `T.isInfixOf` n) name) (attr "alt" t)) named
        img = head (imgUrls sameName) <|> head (imgUrls named) <|> head (imgUrls (filter isImg card))
        soldOut = textHas o.soldOutMarks joined
        price = parseYen =<< priceText
     in if isNothing name && isNothing price
          then Nothing
          else
            Just
              (listOffer ctx u)
                { name = slMaybe "商品名が読めなかった" name
                , priceIncl = if o.taxIncluded then priceField priceText price else missing SearchList "一覧は税抜の表示"
                , priceExcl = if o.taxIncluded then missing SearchList "一覧は税込の表示" else priceField priceText price
                , shipping = case o.shippingClass >>= exact of
                    Just t | Just sh <- parseShipping t -> (sl sh) {raw = Just t}
                    _ -> missing SearchList "一覧に送料の表示なし（個別ページで確かめる）"
                , stock = if soldOut then gotRaw SearchList "売り切れの表示" OutOfStock else missing SearchList "一覧に在庫の表示なし"
                , seller = slMaybe "一覧に販売者の表示なし" (o.sellerName <|> (o.sellerClass >>= exact))
                , image = slMaybe "画像なし" img
                }
  priceField raw = \case
    Just y -> (sl y) {raw = raw}
    Nothing -> missing SearchList "価格が読めなかった"
  isImg = \case
    TagOpen "img" _ -> True
    _ -> False
  -- 「¥1,980」「1,980円」の最初のもの
  firstPrice t =
    let c = toHalfWidth t
        yenMark = snd (T.breakOn "¥" c)
        enMark = T.breakOn "円" c
        -- 「円」の直前の数だけ（手前の評価の数「0.0 0 100円」を巻き込まない）
        beforeEn = T.takeWhileEnd (\ch -> isDigit ch || ch == ',') (T.stripEnd (fst enMark))
     in case (T.null yenMark, T.null (snd enMark)) of
          (False, True) -> Just (T.take 14 yenMark)
          (True, False) | T.any isDigit beforeEn -> Just (beforeEn <> "円")
          (False, False)
            | T.length (fst (T.breakOn "¥" c)) <= T.length (fst enMark) -> Just (T.take 14 yenMark)
            | T.any isDigit beforeEn -> Just (beforeEn <> "円")
            | otherwise -> Just (T.take 14 yenMark)
          _ -> Nothing
