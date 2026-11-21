-- | アダプタの登録簿（カタログの adapter の id → アダプタ）と、URLからサイトを引く関数
module Meguri.Sites
  ( adapterFor
  , siteForUrl
  , matchesItem
  ) where

import Meguri.Catalog
import Meguri.Site.BookOff qualified as BookOff
import Meguri.Site.Captured qualified as Captured
import Meguri.Site.Common (Adapter, noAdapter)
import Meguri.Site.DirectStores qualified as DirectStores
import Meguri.Site.HardOff qualified as HardOff
import Meguri.Site.Jmty qualified as Jmty
import Meguri.Site.Kakaku qualified as Kakaku
import Meguri.Site.Malls qualified as Malls
import Meguri.Site.PayPayFleamarket qualified as PayPayFleamarket
import Meguri.Site.Rakuma qualified as Rakuma
import Meguri.Site.Rakuten qualified as Rakuten
import Meguri.Site.TreasureFactory qualified as TreasureFactory
import Meguri.Site.YahooShopping qualified as YahooShopping
import Meguri.Site.Yahuoku qualified as Yahuoku
import Protolude
import Meguri.Sites.Match (matchesItem)

adapterFor :: Channel -> Adapter
adapterFor s = case s.adapter of
  "rakuten" -> Rakuten.adapter
  "yahoo-shopping" -> YahooShopping.adapter
  "kakaku" -> Kakaku.adapter
  "yahuoku" -> Yahuoku.adapter
  "paypayfleamarket" -> PayPayFleamarket.adapter
  "rakuma" -> Rakuma.adapter
  "hardoff" -> HardOff.adapter
  "treasure-factory" -> TreasureFactory.adapter
  "jmty" -> Jmty.adapter
  "bookoff" -> BookOff.adapter
  "loft" -> DirectStores.loft
  "francfranc" -> DirectStores.francfranc
  "kohnan" -> DirectStores.kohnan
  "dinos" -> DirectStores.dinos
  "japanet" -> DirectStores.japanet
  "iosys" -> DirectStores.iosys
  "cainz" -> DirectStores.cainz
  "costco" -> DirectStores.costco
  "ikea" -> DirectStores.ikea
  "sofmap" -> DirectStores.sofmap
  "hands" -> DirectStores.hands
  "ishibashi" -> DirectStores.ishibashi
  "lohaco" -> Malls.lohaco
  "aupay" -> Malls.aupay
  "buyma" -> Malls.buyma
  "locondo" -> Malls.locondo
  "cado" -> DirectStores.cado
  "okoku" -> DirectStores.okoku
  "akachan" -> DirectStores.akachan
  "yodobashi" -> Captured.yodobashi
  "amazon" -> Captured.amazon
  "mercari" -> Captured.mercari
  "2ndstreet" -> Captured.secondStreet
  "yamada" -> Captured.yamada
  "ksdenki" -> Captured.ksdenki
  "nitori" -> Captured.nitori
  "janpara" -> Captured.janpara
  "ebay" -> Captured.ebay
  _ -> noAdapter

-- | 個別ページのURLの形から、サイトを引く。
-- 支店（2nd STREET の楽天店など）は本店より形が細かいので、先に当たったものを優先する。
siteForUrl :: Text -> Maybe Channel
siteForUrl u =
  let matches = filter (`matchesItem` u) catalog
      branch = filter (\s -> s.adapter /= s.sid) matches
   in head branch <|> head matches
