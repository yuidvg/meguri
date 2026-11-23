-- | ship：送料を計算できるサイトで、配送先までの送料を出す。
--
-- 配送先（MEGURI_PREF）は、実行の記録に残さない。
module Meguri.Ship
  ( Pref (..)
  , parsePref
  , ShipPlan (..)
  , shipPlan
  , supported
  , applyShipping
  ) where

import Data.Aeson (Value (..))
import Data.Text qualified as T
import Meguri.Confirm (judge)
import Meguri.Extract.JsonLd (at, decodeLenient, str)
import Meguri.Extract.Price (parseYen)
import Meguri.Fetch.Http (Req (..))
import Meguri.Sites (siteForUrl)
import Meguri.Text (digitsOnly)
import Meguri.Catalog (Channel (..))
import Meguri.Types
import Protolude

-- | 配送先の都道府県（JIS X 0401 のコードと名前）
data Pref = Pref
  { code :: Int
  , label :: Text
  }
  deriving stock (Eq, Show)

prefNames :: [Text]
prefNames =
  [ "北海道", "青森県", "岩手県", "宮城県", "秋田県", "山形県", "福島県", "茨城県", "栃木県", "群馬県"
  , "埼玉県", "千葉県", "東京都", "神奈川県", "新潟県", "富山県", "石川県", "福井県", "山梨県", "長野県"
  , "岐阜県", "静岡県", "愛知県", "三重県", "滋賀県", "京都府", "大阪府", "兵庫県", "奈良県", "和歌山県"
  , "鳥取県", "島根県", "岡山県", "広島県", "山口県", "徳島県", "香川県", "愛媛県", "高知県", "福岡県"
  , "佐賀県", "長崎県", "熊本県", "大分県", "宮崎県", "鹿児島県", "沖縄県"
  ]

-- | 「13」「東京都」「東京」のどれでも読む
parsePref :: Text -> Maybe Pref
parsePref raw =
  let t = T.strip raw
      byCode = do
        n <- readMaybe t :: Maybe Int
        name <- atMay prefNames (n - 1)
        guard (n >= 1)
        pure (Pref n name)
      byName = head [Pref n name | (n, name) <- zip [1 ..] prefNames, name == t || T.dropEnd 1 name == t]
   in if T.all (`elem` ("0123456789" :: [Char])) t && not (T.null t) then byCode else byName

-- | 送料の問い合わせ：何を取りに行き、応答からどう読むか
data ShipPlan = ShipPlan
  { req :: Req
  , -- | 応答の本文 → 送料
    readFee :: Text -> Maybe Yen
  }

supported :: [Text]
supported = ["hardoff", "treasure-factory"]

-- | 個別ページのURLから、送料の問い合わせを作る（純粋）。対応していないサイトは 'Nothing'。
shipPlan :: Pref -> Text -> Maybe ShipPlan
shipPlan pref url = case (.sid) <$> siteForUrl url of
  -- POST /ajax/CHfOmCalcDelvCom_J.jsp（toPref＝都道府県名、goodsNo）→ {"result":"SUCCESS","delvCom":"600"}
  Just "hardoff" -> do
    goods <- idAfter "/product/"
    pure
      ShipPlan
        { req = Req "https://netmall.hardoff.co.jp/ajax/CHfOmCalcDelvCom_J.jsp" (Just [("toPref", pref.label), ("goodsNo", goods)]) True
        , readFee = \body -> do
            v <- decodeLenient body
            guard (at "result" v == Just (String "SUCCESS"))
            parseYen =<< str =<< at "delvCom" v
        }
  -- GET /item/postage?code={商品番号}&prefId={都道府県コード} → {"postage":"1,050","result":true}
  Just "treasure-factory" -> do
    code <- idAfter "/item/"
    pure
      ShipPlan
        { req = Req ("https://ec.treasure-f.com/item/postage?code=" <> code <> "&prefId=" <> show pref.code) Nothing True
        , readFee = \body -> do
            v <- decodeLenient body
            guard (at "result" v == Just (Bool True))
            parseYen =<< str =<< at "postage" v
        }
  _ -> Nothing
 where
  idAfter marker =
    let rest = snd (T.breakOn marker url)
        ds = digitsOnly (T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') (T.drop (T.length marker) rest))
     in if T.null rest || T.null ds then Nothing else Just ds

-- | 記録の送料を、サイトの送料計算の結果に置き換え、判定をやり直す
applyShipping :: Yen -> Reading -> Reading
applyShipping fee r =
  let field = (got (ItemPage ShippingCalc) (if fee == 0 then Free else Fixed fee)) {note = Just "サイトの送料計算（MEGURI_PREF の配送先まで）"}
      o = r.offer {shipping = field}
   in Reading {offer = o, adapter = r.adapter, confirmation = judge r.adapter o}
