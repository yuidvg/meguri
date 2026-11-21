-- | ヤフオク（検索・落札相場・個別）。値は __NEXT_DATA__ にある。
module Meguri.Site.Yahuoku (adapter) where

import Data.Aeson (Value (..))
import Data.Text qualified as T
import Meguri.Extract.JsonLd (arr, at, atPath, int, str)
import Meguri.Extract.NextData (nextData)
import Meguri.Extract.Html (Tags)
import Meguri.Site.Common
import Meguri.Types
import Protolude

adapter :: Adapter
adapter = Adapter (Just listPage) (Just itemPage)

listPage :: ListCtx -> Tags -> Text -> ListOut
listPage ctx ts _ = case atPath ["props", "pageProps", "initialState", "search", "items", "listing"] =<< nextData ts of
  Nothing -> ListOut [] False []
  Just listing ->
    let items = maybe [] arr (at "items" listing)
     in ListOut
          { offers = mapMaybe one items
          , zero = null items
          , follow = []
          }
 where
  closed = "closedsearch" `T.isInfixOf` ctx.pageUrl
  one v = do
    aid <- str =<< at "auctionId" v
    let b k = at k v == Just (Bool True)
        price = Yen <$> (int =<< at "price" v)
        buyNow = int =<< at "buyNowPrice" v
        fee = int =<< at "currentPriceShippingFee" v
        feeType = str =<< at "currentPriceShippingFeeType" v
        ends = fromMaybe "" (str =<< at "endTime" v)
        bidCount = fromMaybe 0 (int =<< at "bidCount" v)
    pure
      (listOffer ctx ("https://auctions.yahoo.co.jp/jp/auction/" <> aid))
        { name = slMaybe "商品名が読めなかった" (str =<< at "title" v)
        , priceIncl =
            (if closed then withNote "落札価格" else maybe identity (\n -> withNote ("即決 " <> show n <> "円")) buyNow)
              (slMaybe "価格が読めなかった" price)
        , shipping =
            if b "isFreeShipping"
              then sl Free
              else case fee of
                Just n | feeType == Just "PREFECTURE" -> withNote ("一覧の表示は " <> show n <> "円（配送先で変わる）") (gotRaw SearchList (show n) ByDestination)
                Just n -> gotRaw SearchList (fromMaybe "" feeType) (Fixed (Yen n))
                Nothing -> withNote "落札者負担。額は一覧に無い" (sl BuyerPays)
        , stock =
            sl $
              if closed
                then Sold
                else if b "isFixedPrice" then Selling else Auction {bids = bidCount, endsAt = ends}
        , seller = slMaybe "出品者名は一覧では伏せ字" (mfilter (T.any (/= '*')) (str =<< atPath ["seller", "displayName"] v))
        , image = slMaybe "画像なし" (T.takeWhile (/= '?') <$> (str =<< at "imageUrl" v))
        , condition = slMaybe "状態の表示なし" (str =<< at "itemCondition" v)
        , -- Yahoo!フリマの出品（z…）は、Yahoo!フリマの検索にも同じ番号で出る
          keys = noKeys {siteItemId = Just aid, sharedStockId = if b "isFleamarketItem" || "z" `T.isPrefixOf` aid then Just aid else Nothing}
        }

itemPage :: ItemCtx -> Draft -> Draft
itemPage ctx d = case atPath ["props", "pageProps", "initialState", "item", "detail", "item"] =<< nextData ctx.tags of
  Nothing -> d
  Just v ->
    let s k = str =<< at k v
        n k = int =<< at k v
        status = s "status"
        open = status == Just "open"
        ends = fromMaybe "" (s "endTime")
        buyNow = n "taxinBidorbuy" <|> n "bidorbuy"
        methods = mapMaybe (str <=< at "name") (maybe [] arr (atPath ["shipping", "methods"] v))
     in d
          { name = bodyField NextData "title が無い" (s "title") `orField` d.name
          , priceIncl =
              maybe identity (\b -> withNote ("即決 " <> show b <> "円")) buyNow $
                withNote "現在価格。落札できた場合の額" $
                  bodyField NextData "price が無い" (Yen <$> (n "taxinPrice" <|> n "price"))
          , shipping = case s "chargeForShipping" of
              Just "seller" -> gotRaw NextData "chargeForShipping:seller" Free
              Just "winner" ->
                withNote ("落札者負担。" <> (if null methods then "額は未確認" else T.intercalate "・" methods <> "（額は配送先で変わる）")) $
                  gotRaw NextData "chargeForShipping:winner" (if null methods then BuyerPays else ByDestination)
              _ -> missing NextData "chargeForShipping が無い"
          , stock = case status of
              Just "open" -> got NextData (Auction {bids = fromMaybe 0 (n "bids"), endsAt = ends})
              Just other -> gotRaw NextData other Sold
              Nothing -> missing NextData "status が無い"
          , seller = bodyField NextData "出品者名なし" (str =<< atPath ["seller", "displayName"] v)
          , image = bodyField NextData "画像なし" (str =<< (at "image" =<< head (maybe [] arr (at "img" v)))) `orField` d.image
          , buy =
              if open
                then got NextData (BuyActive (if isJust buyNow then "入札する／今すぐ落札する" else "入札する"))
                else got NextData (BuyDisabled "終了")
          , condition = bodyField NextData "状態の表示なし" (s "conditionName")
          , keys =
              d.keys
                { siteItemId = s "auctionId"
                , -- Yahoo!フリマの出品（z…）は、Yahoo!フリマにも同じ番号で出ている
                  sharedStockId = mfilter (\i -> at "isFleaMarket" v == Just (Bool True) || "z" `T.isPrefixOf` i) (s "auctionId")
                }
          }
