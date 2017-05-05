module Data.Integer.Bounded.Nat (NatBelow, below, add, mul) where

import Prelude
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num.Sets (class Nat, class Pos, toInt)
import Data.Typelevel.Num.Ops (class Succ, class Pred, class Trich, class Add, class Mul, class LtEq, GT, LT)
import Data.Typelevel.Undefined (undefined)

newtype NatBelow n = Bounded Int

derive instance eqNatBelow :: Eq (NatBelow n)
derive instance ordNatBelow :: Ord (NatBelow n)
instance boundedNatBelow :: Pos n => Bounded (NatBelow n) where
  bottom = Bounded 0
  top = Bounded $ toInt (undefined :: n) - 1
instance enumNatBelow :: Pos n => Enum (NatBelow n) where
  pred n
     | n <= bottom      = Nothing
     | (Bounded i) <- n = Just $ Bounded $ i - 1
  succ n
     | n >= top         = Nothing
     | (Bounded i) <- n = Just $ Bounded $ i + 1
instance boundedEnumNatBelow :: Pos n => BoundedEnum (NatBelow n) where
  cardinality = Cardinality $ toInt (undefined :: n)
  fromEnum = toInt
  toEnum = below

instance natNatBelow :: Pos n => Nat (NatBelow n)
  where toInt (Bounded i) = i
instance trichNatBelowGT :: (Pos n, Pos n', LtEq n' n) 
                         => Trich n (NatBelow n') GT
instance trichNatBelowLT :: (Pos n, Pos n', LtEq n' n)
                         => Trich (NatBelow n') n LT

below :: forall n. Pos n => Int -> Maybe (NatBelow n)
below i = if i >= 0 && i < toInt (undefined :: n)
          then Just (Bounded i)
          else Nothing

add :: forall n1 n2 n3 n3'. Pos n1 => Pos n2 => Add n1 n2 n3 => Pred n3 n3'
       => NatBelow n1 -> NatBelow n2 -> NatBelow n3'
add (Bounded x) (Bounded y) = Bounded $ x + y

mul :: forall n1 n1' n2 n2' n3 n3'. Pos n1 => Pos n2 => Pred n1 n1' => Pred n2 n2' => Mul n1' n2' n3 => Succ n3 n3'
       => NatBelow n1 -> NatBelow n2 -> NatBelow n3'
mul (Bounded x) (Bounded y) = Bounded $ x * y
