module Flare
  ( Component()
  , Cell()
  , Flare(..)
  , ElementId()
  , lift
--  , foldp
  , number
  , number_
  , numberRange
  , numberRange_
  , int
  , int_
  , intRange
  , intRange_
  , string
  , string_
  , boolean
  , boolean_
  , button
--  , select
--  , select_
  , cellToUI -- TODO
  , SetupUI(..) -- TODO
  , UI(..) -- TODO
  , appendComponent -- TODO
  , runFlare
  ) where

import Prelude

import Data.Array (reverse)
import Data.Foldable (traverse_)
import Data.Monoid
import Data.Traversable (traverse)

import Control.Apply
import Control.Monad.Eff
import Control.Applicative.Free

import DOM
import DOM.Node.Types (Element())

import qualified Signal as S
import Signal.Channel

type ElementId = String
type Label = String

data UI a = UI (Array Element) (S.Signal a)

instance functorUI :: Functor UI where
  map f (UI cs sig) = UI cs (map f sig)

instance applyUI :: Apply UI where
  apply (UI cs1 sig1) (UI cs2 sig2) = UI (cs1 <> cs2) (sig1 <*> sig2)

instance applicativeUI :: Applicative UI where
  pure x = UI [] (pure x)

newtype SetupUI e a = SetupUI (Eff (dom :: DOM, chan :: Chan | e) (UI a))

instance functorSetupUI :: Functor (SetupUI e) where
  map f (SetupUI a) = SetupUI $ map (map f) a

instance applySetupUI :: Apply (SetupUI e) where
  apply (SetupUI a1) (SetupUI a2) = SetupUI $ lift2 apply a1 a2

instance applicativeSetupUI :: Applicative (SetupUI e) where
  pure x = SetupUI $ return (pure x)

data Component
  = CNumber Label Number
  | CNumberRange Label Number Number Number Number
  | CInt Label Int
  | CIntRange Label Int Int Int
  | CString Label String
  | CBoolean Label Boolean
  | CButton Label

data Cell a = Cell (Array Component) a
            | Lift (S.Signal a)

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

-- | Lift a `Signal` to a `Flare`.
lift :: forall a. S.Signal a -> Flare a
lift sig = Flare (liftFreeAp (Lift sig))

{--

-- | Create a past dependent component. The fold-function takes the current
-- | value of the component and the previous value of the output to produce
-- | the new value of the output.
foldp :: forall a b e. (a -> b -> b) -> b -> UI e a -> UI e b
foldp f x0 (UI setup) = UI $ do
  (Flare comp sig) <- setup
  return $ Flare comp (S.foldp f x0 sig)

--}

type UpdateHandler a = a -> Eff (chan :: Chan) Unit

-- | Append a child element to the parent with the specified ID
foreign import appendComponent :: forall e. ElementId
                               -> Element -> Eff (dom :: DOM | e) Unit

-- | Set the inner HTML of the specified element to the given value
foreign import renderString :: forall e. ElementId
                            -> String
                            -> Eff (dom :: DOM | e) Unit

type CreateComponent a = forall a b e. Label
                         -> a
                         -> UpdateHandler b
                         -> Eff (dom :: DOM, chan :: Chan | e) Element

foreign import cNumber :: CreateComponent Number
foreign import cNumberRange :: Number -> Number -> Number -> CreateComponent Number
foreign import cInt :: CreateComponent Int
foreign import cIntRange :: Int -> Int -> CreateComponent Int
foreign import cString :: CreateComponent String
foreign import cBoolean :: CreateComponent Boolean
foreign import cButton :: CreateComponent Boolean
foreign import cSelect :: forall a. (Show a) => Array a -> CreateComponent a

makeFlare :: forall a. Component -> a -> Flare a
makeFlare component default = Flare (liftFreeAp (Cell [component] default))

-- | Creates a text field for a `Number` input from a given label and default
-- | value.
number :: Label -> Number -> Flare Number
number label default = makeFlare (CNumber label default) default

-- | Creates a text field for a `Number` input with a default value.
number_ :: Number -> Flare Number
number_ = number ""

-- | Creates a slider for a `Number` input from a given label,
-- | minimum value, maximum value, step size as well as default value.
numberRange :: Label -> Number -> Number -> Number -> Number -> Flare Number
numberRange label min max step default =
  makeFlare (CNumberRange label min max step default) default

-- | Creates a slider for a `Number` input without a label.
numberRange_ :: Number -> Number -> Number -> Number -> Flare Number
numberRange_ = numberRange ""

-- | Creates a text field for an `Int` input from a given label and default
-- | value.
int :: Label -> Int -> Flare Int
int label default = makeFlare (CInt label default) default

-- | Creates a text field for an `Int` input with a default value.
int_ :: Int -> Flare Int
int_ = int ""

-- | Creates a slider for an `Int` input from a given label, minimum and
-- | maximum values as well as a default value.
intRange :: Label -> Int -> Int -> Int -> Flare Int
intRange label min max default =
  makeFlare (CIntRange label min max default) default

-- | Creates a slider for an `Int` input without a label.
intRange_ :: Int -> Int -> Int -> Flare Int
intRange_ = intRange ""

-- | Creates a text field for a `String` input from a given label and default
-- | value.
string :: Label -> String -> Flare String
string label default = makeFlare (CString label default) default

{-- -- | Creates a text field for a `String` input with a default value. --}
string_ :: String -> Flare String
string_ = string ""

-- | Creates a checkbox for a `Boolean` input from a given label and default
-- | value.
boolean :: Label -> Boolean -> Flare Boolean
boolean label default = makeFlare (CBoolean label default) default

-- | Creates a checkbox for a `Boolean` input with a default value.
boolean_ :: Boolean -> Flare Boolean
boolean_ = boolean ""

-- | Creates a button which yields `true` if is pressed and `false` otherwise.
button :: Label -> Flare Boolean
button label = makeFlare (CButton label) false

{--

-- | Creates a select box to choose from a list of options. The first option
-- | is selected by default. The rest of the options is given as an array.
select :: forall a. (Show a) => Label -> a -> Array a -> Flare a
select label default xs = ...

-- | Create a select box without a label.
select_ :: forall e a. (Show a) => a -> Array a -> UI e a
select_ = select ""

--}

cellToUI :: forall e. NaturalTransformation Cell (SetupUI e)
cellToUI (Cell components x) = SetupUI $ do
  chan <- channel x
  elements <- traverse (toElement (send chan)) components
  return (UI elements (subscribe chan))
cellToUI (Lift sig) = SetupUI $ return (UI [] sig)

toElement :: forall a e. UpdateHandler a
          -> Component
          -> Eff (dom :: DOM, chan :: Chan | e) Element
toElement send (CNumber label default) =
  cNumber label default send
toElement send (CNumberRange label min max step default) =
  cNumberRange min max step label default send
toElement send (CInt label default) =
  cInt label default send
toElement send (CIntRange label min max default) =
  cIntRange min max label default send
toElement send (CString label default) =
  cString label default send
toElement send (CBoolean label default) =
  cBoolean label default send
toElement send (CButton label) =
  cButton label false send

-- | Renders a `Flare` to the DOM and sets up all event handlers. The two IDs
-- | specify the DOM elements to which the controls and the output will be
-- | attached, respectively.
runFlare :: forall e a. (Show a)
         => ElementId
         -> ElementId
         -> Flare a
         -> Eff (dom :: DOM, chan :: Chan | e) Unit
runFlare controls target (Flare flare) =
  case foldFreeAp cellToUI flare of
    (SetupUI setup) -> do
      (UI els sig) <- setup
      traverse_ (appendComponent controls) (reverse els)
      S.runSignal (map (show >>> renderString target) sig)
