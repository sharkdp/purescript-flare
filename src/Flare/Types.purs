module Flare.Types
  ( Label(..)
  , ElementId(..)
  , Component(..)
  , Cell(..)
  , Flare(..)
  ) where

import Prelude

import Data.Monoid

import Control.Apply
import Control.Applicative.Free

import Signal

-- | A text label for control elements
type Label = String

-- | The ID of a DOM element
type ElementId = String

-- | Possible input types.
data Component
  = CNumber Label Number
  | CNumberRange Label Number Number Number Number
  | CInt Label Int
  | CIntRange Label Int Int Int
  | CString Label String
  | CBoolean Label Boolean
  | CButton Label

-- | Intermediate data type.
data Cell a = Cell (Array Component) a
            | Lift (Signal a)
            | FoldP (forall b. b -> a -> a) a (forall c. Flare c)

-- | The main data type for a Flare UI.
data Flare a = Flare (FreeAp Cell a)

instance functorFlare :: Functor Flare where
  map f (Flare x) = Flare (map f x)

instance applyFlare :: Apply Flare where
  apply (Flare x) (Flare y) = Flare (x <*> y)

instance applicativeFlare :: Applicative Flare where
  pure = Flare <<< pure

instance semigroupFlare :: (Semigroup a) => Semigroup (Flare a) where
  append = lift2 append

instance monoidFlare :: (Monoid a) => Monoid (Flare a) where
  mempty = pure mempty

instance semiringFlare :: (Semiring a) => Semiring (Flare a) where
  one = pure one
  mul = lift2 mul
  zero = pure zero
  add = lift2 add

instance ringFlare :: (Ring a) => Ring (Flare a) where
  sub = lift2 sub

instance moduloSemiringFlare :: (ModuloSemiring a) => ModuloSemiring (Flare a) where
  mod = lift2 mod
  div = lift2 div

instance divisionRingFlare :: (DivisionRing a) => DivisionRing (Flare a)

instance numFlare :: (Num a) => Num (Flare a)

instance boundedFlare :: (Bounded a) => Bounded (Flare a) where
  top = pure top
  bottom = pure bottom

instance booleanAlgebraFlare :: (BooleanAlgebra a) => BooleanAlgebra (Flare a) where
  conj = lift2 conj
  disj = lift2 disj
  not = map not
