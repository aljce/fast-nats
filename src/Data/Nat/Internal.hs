{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Data.Nat.Internal
  ( type Nat -- | Inductive Type Level Naturals
  ) where

import Data.Kind (Type)
import qualified GHC.TypeLits as GHC
import Data.Type.Equality ((:~:)(..))
import Data.Singletons (Sing(..))
import Data.Singletons.Prelude (PNum(..), PEnum(..), PEq(..), POrd(..))
import Data.Singletons.Decide (SDecide(..),Decision(..))

import Unsafe.Coerce (unsafeCoerce)

import Data.Viewable (Viewable(..))

data Nat
  = Zero
  | Succ Nat

type family FromKnownNat (n :: GHC.Nat) :: Nat where
  FromKnownNat 0 = Zero
  FromKnownNat n = Succ (FromKnownNat (n - 1))

type family ToKnownNat (n :: Nat) :: GHC.Nat where
  ToKnownNat Zero = 0
  ToKnownNat (Succ n) = 1 + ToKnownNat n

instance PNum Nat where
  type Zero + n = n
  type (Succ n) + m = Succ (n + m)
  type n - Zero = n
  type Zero - _ = Zero
  type (Succ n) - (Succ m) = n - m
  type Zero * m = Zero
  type (Succ n) * m = m + (n * m)
  type Negate m = GHC.TypeError (GHC.Text "Cannot negate a natural number")
  type Abs n = n
  type Signum Zero = Zero
  type Signum (Succ _) = Succ Zero
  type FromInteger n = FromKnownNat n

instance PEnum Nat where
  type ToEnum n = FromKnownNat n
  type FromEnum n = ToKnownNat n

instance PEq Nat where
  type Zero == Zero = True
  type Zero == (Succ _) = False
  type (Succ _) == Zero = False
  type (Succ n) == (Succ m) = n == m
  type Zero /= Zero = False
  type Zero /= (Succ _) = True
  type (Succ _) /= Zero = True
  type (Succ n) /= (Succ m) = n /= m

instance POrd Nat where
  type Compare Zero Zero = EQ
  type Compare (Succ _) Zero = GT
  type Compare Zero (Succ _) = LT
  type Compare (Succ n) (Succ m) = Compare n m
  type Zero < Zero = False
  type Zero < (Succ _) = True
  type (Succ _) < Zero = False
  type (Succ n) < (Succ m) = n < m
  type Zero <= Zero = True
  type Zero <= (Succ _) = True
  type (Succ _) <= Zero = False
  type (Succ n) <= (Succ m) = n < m
  type Zero > Zero = False
  type Zero > (Succ _) = False
  type (Succ _) > Zero = True
  type (Succ n) > (Succ m) = n > m
  type Zero >= Zero = True
  type Zero >= (Succ _) = False
  type (Succ _) >= Zero = True
  type (Succ n) >= (Succ m) = n > m
  type Max Zero m = m
  type Max (Succ n) Zero = Succ n
  type Max (Succ n) (Succ m) = Succ (Max n m)
  type Min Zero _ = Zero
  type Min _ Zero = Zero
  type Min (Succ n) (Succ m) = Succ (Min n m)

newtype instance Sing (n :: Nat) = SNat { word :: Word }

instance Viewable Nat where
  data View :: Nat -> Type where
    VZero :: View Zero
    VSucc :: Sing n -> View (Succ n)
  view :: Sing (n :: Nat) -> View n
  view (SNat 0) = unsafeCoerce VZero
  view (SNat n) = unsafeCoerce (VSucc (SNat (n - 1)))

instance Show (Sing (n :: Nat)) where
  showsPrec p n = showsPrec p (word n)

cong :: forall (f :: i -> j) (a :: i) (b :: i). a :~: b -> f a :~: f b
cong Refl = Refl

succInjective :: Succ n :~: Succ m -> n :~: m
succInjective Refl = Refl

instance SDecide Nat where
  n1 %~ m1 = case view n1 of
    VZero -> case view m1 of
      VZero   -> Proved Refl
      VSucc _ -> Disproved (\case{})
    VSucc n2 -> case view m1 of
      VZero    -> Disproved (\case{})
      VSucc m2 -> case n2 %~ m2 of
        Proved prf -> Proved (cong @_ @_ @Succ prf)
        Disproved contra -> Disproved (contra . succInjective)
  -- (SNat n) %~ (SNat m)
  --   | n == m = Proved (unsafeCoerce Refl)
  --   | otherwise = Disproved (\Refl -> error "Data.Singletons.SDecide Nat: Int equality failed.")

