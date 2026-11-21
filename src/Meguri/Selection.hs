{-# LANGUAGE DeriveAnyClass #-}

-- | 選び方：並びの中から、条件を全部満たすものを選ぶ。
--
-- 選び方は2種類の部品でできている。
--
-- * 条件（'Criterion'）：1件ごとに決まる、純粋な条件。何度でもかけ直せる。
-- * 判定（'Judge'）：判断を外に任せる。外のコマンドか、判定モデル（jev）への質問。まとめて1回だけ呼ぶ。
--
-- 選ばれるのは「条件を全部満たし、判定が全部残すと答えたもの」で、書く順によらない。
-- かける順は決まっている：条件を先に（書いた順）、判定を後に（書いた順）。安い方を先にするため。
--
-- 何を選ぶかは型の引数で決まる。販路を選ぶ（"Meguri.Scope"）のにも、見つけた売り物を選ぶ（"Meguri.Query"）のにも使う。
module Meguri.Selection
  ( Selection (..)
  , Criterion (..)
  , Judge (..)
  , JudgeBy (..)
  , Ask
  , Selected (..)
  , Excluded (..)
  , everything
  , labels
  , sift
  , select
  , addExcluded
  ) where

import Data.Aeson (ToJSON, Value)
import Data.List (partition)
import Data.Set qualified as Set
import Protolude

-- | 条件：名前（出力に出す）と、満たすかどうか
data Criterion a = Criterion
  { label :: Text
  , holds :: a -> Bool
  }

-- | 判定：名前（出力に出す）と、誰に任せるか
data Judge a = Judge
  { label :: Text
  , by :: JudgeBy
  }

data JudgeBy
  = -- | 外のコマンド（シェルに渡す1行）
    Command Text
  | -- | 判定モデル jev への、はい／いいえの質問
    Jev Text
  deriving stock (Eq, Show)

-- | 判定を頼む：見る値を JSON の行（@id@ つき）にして渡し、残すものの id を受け取る。実体は "Meguri.Judge"
type Ask = JudgeBy -> [Value] -> IO (Either Text (Set Text))

data Selection a = Selection
  { criteria :: [Criterion a]
  , judges :: [Judge a]
  }

instance Semigroup (Selection a) where
  a <> b = Selection (a.criteria <> b.criteria) (a.judges <> b.judges)

instance Monoid (Selection a) where
  mempty = everything

-- | 何も外さない
everything :: Selection a
everything = Selection [] []

-- | かける順に並べた名前
labels :: Selection a -> [Text]
labels s = map (.label) s.criteria <> map (.label) s.judges

-- | どの条件・判定が、いくつ外したか
data Excluded = Excluded
  { by :: Text
  , count :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | 選んだ結果
data Selected a = Selected
  { chosen :: [a]
  , excluded :: [Excluded]
  }
  deriving stock (Eq, Show, Functor)

-- | 条件だけをかける（純粋）
sift :: [Criterion s] -> (a -> s) -> [a] -> Selected a
sift cs view xs0 = foldl' step (Selected xs0 []) cs
 where
  step (Selected xs ex) c =
    let (yes, no) = partition (c.holds . view) xs
     in Selected yes (ex <> [Excluded c.label (length no)])

-- | 条件をかけ、残ったものを判定に回す。判定が失敗したら、そこでやめて理由を返す
-- （頼まれた判定を、かけなかったことにして先へ進まない）。
--
-- 2番目の引数は、判定に渡す行の作り方：見る値の並びから、(id, JSON の行) の並びを作る。
select :: Ask -> ([s] -> [(Text, Value)]) -> Selection s -> (a -> s) -> [a] -> IO (Either Text (Selected a))
select ask rows sel view xs0 = go sel.judges (sift sel.criteria view xs0)
 where
  go [] done = pure (Right done)
  go (j : rest) (Selected xs ex)
    | null xs = go rest (Selected xs (ex <> [Excluded j.label 0]))
    | otherwise = do
        let sent = rows (map view xs)
        r <- ask j.by (map snd sent)
        case r of
          Left e -> pure (Left (j.label <> "：" <> e))
          Right ids ->
            let yes = [x | ((i, _), x) <- zip sent xs, i `Set.member` ids]
             in go rest (Selected yes (ex <> [Excluded j.label (length xs - length yes)]))

-- | 同じ選び方を、分けてかけた結果の「外した数」を足す
addExcluded :: [Excluded] -> [Excluded] -> [Excluded]
addExcluded = zipWith (\a b -> a {count = a.count + b.count})
