module Data.Integer.Bounded.Pos (PosBelow, below, add, mul) where

import Prelude
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num.Sets (class Nat, class Pos, toInt)
import Data.Typelevel.Num.Ops (class Succ, class Pred, class Trich, class Add, class Mul, class LtEq, GT, LT)
import Data.Typelevel.Undefined (undefined)

newtype PosBelow n = Bounded Int

derive instance eqPosBelow :: Eq (PosBelow n)
derive instance ordPosBelow :: Ord (PosBelow n)
instance boundedPosBelow :: Pos n => Bounded (PosBelow n) where
  bottom = Bounded 1
  top = Bounded $ toInt (undefined :: n) - 1
instance enumPosBelow :: Pos n => Enum (PosBelow n) where
  pred n
     | n <= bottom      = Nothing
     | (Bounded i) <- n = Just $ Bounded $ i - 1
  succ n
     | n >= top         = Nothing
     | (Bounded i) <- n = Just $ Bounded $ i + 1
instance boundedEnumPosBelow :: Pos n => BoundedEnum (PosBelow n) where
  cardinality = Cardinality $ toInt (undefined :: n)
  fromEnum = toInt
  toEnum = below

instance natPosBelow :: Pos n => Nat (PosBelow n)
  where toInt (Bounded i) = i
instance trichPosBelowGT :: (Pos n, Pos n', LtEq n' n) 
                         => Trich n (PosBelow n') GT
instance trichPosBelowLT :: (Pos n, Pos n', LtEq n' n)
                         => Trich (PosBelow n') n LT

below :: forall n. Pos n => Int -> Maybe (PosBelow n)
below i = if i > 0 && i < toInt (undefined :: n)
          then Just (Bounded i)
          else Nothing

add :: forall n1 n2 n3 n3'. Pos n1 => Pos n2 => Add n1 n2 n3 => Pred n3 n3'
       => PosBelow n1 -> PosBelow n2 -> PosBelow n3'
add (Bounded x) (Bounded y) = Bounded $ x + y

mul :: forall n1 n1' n2 n2' n3 n3'. Pos n1 => Pos n2 => Pred n1 n1' => Pred n2 n2' => Mul n1' n2' n3 => Succ n3 n3'
       => PosBelow n1 -> PosBelow n2 -> PosBelow n3'
mul (Bounded x) (Bounded y) = Bounded $ x * y
