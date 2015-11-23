module Flare
  ( Flare(..)
  , UI(..)
  , ElementId()
  , lift
  , wrap
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
  , select
  , select_
  , appendComponents
  , runFlare
  ) where

import Prelude

import Data.Monoid
import Data.Foldable (traverse_)

import Control.Apply
import Control.Monad.Eff

import DOM
import DOM.Node.Types (Element())

import Signal
import Signal.Channel

type ElementId = String
type Label = String

-- | A `Flare` is a `Signal` with a corresponding list of HTML elements
-- | for the user interface components.
data Flare a = Flare (Array Element) (Signal a)

-- | The main data type for a Flare UI. It encapsulates the `Eff` action
-- | which is to be run when setting up the input elements and corresponding
-- | signals.
newtype UI e a = UI (Eff (dom :: DOM, chan :: Chan | e) (Flare a))

instance functorUI :: Functor (UI e) where
  map f (UI a) = UI $ do
    (Flare cs sig) <- a
    return $ Flare cs (map f sig)

instance applyUI :: Apply (UI e) where
  apply (UI a1) (UI a2) = UI $ do
    (Flare cs1 sig1) <- a1
    (Flare cs2 sig2) <- a2
    return $ Flare (cs1 <> cs2) (apply sig1 sig2)

instance applicativeUI :: Applicative (UI e) where
  pure x = UI $ return (Flare [] (pure x))

instance semigroupUI :: (Semigroup a) => Semigroup (UI e a) where
  append = lift2 append

instance monoidUI :: (Monoid a) => Monoid (UI e a) where
  mempty = pure mempty

instance semiringUI :: (Semiring a) => Semiring (UI e a) where
  one = pure one
  mul = lift2 mul
  zero = pure zero
  add = lift2 add

instance ringUI :: (Ring a) => Ring (UI e a) where
  sub = lift2 sub

instance moduloSemiringUI :: (ModuloSemiring a) => ModuloSemiring (UI e a) where
  mod = lift2 mod
  div = lift2 div

instance divisionRingUI :: (DivisionRing a) => DivisionRing (UI e a)

instance numUI :: (Num a) => Num (UI e a)

instance boundedUI :: (Bounded a) => Bounded (UI e a) where
  top = pure top
  bottom = pure bottom

instance booleanAlgebraUI :: (BooleanAlgebra a) => BooleanAlgebra (UI e a) where
  conj = lift2 conj
  disj = lift2 disj
  not = map not

-- | Lift a `Signal` inside the `Eff` monad to a `UI` component.
lift :: forall e a. Eff (chan :: Chan, dom :: DOM | e) (Signal a) -> UI e a
lift msig = UI $ do
  sig <- msig
  return $ Flare [] sig

-- | Encapsulte a `Signal` within a `UI` component.
wrap :: forall e a. (Signal a) -> UI e a
wrap sig = UI $ return $ Flare [] sig

-- | Append a child element to the parent with the specified ID
foreign import appendComponent :: forall e. ElementId
                               -> Element -> Eff (dom :: DOM | e) Unit

-- | Set the inner HTML of the specified element to the given value
foreign import renderString :: forall e. ElementId
                            -> String
                            -> Eff (dom :: DOM | e) Unit

type CreateComponent a = forall e. Label
                         -> a
                         -> (a -> Eff (chan :: Chan) Unit)
                         -> Eff (dom :: DOM, chan :: Chan | e) Element

foreign import cNumber :: CreateComponent Number
foreign import cNumberRange :: Number -> Number -> Number -> CreateComponent Number
foreign import cInt :: CreateComponent Int
foreign import cIntRange :: Int -> Int -> CreateComponent Int
foreign import cString :: CreateComponent String
foreign import cBoolean :: CreateComponent Boolean
foreign import cSelect :: forall a. (Show a) => Array a -> CreateComponent a

-- | Set up the HTML element for a given component and create the corresponding
-- | signal channel.
createUI :: forall e a. (CreateComponent a) -> Label -> a -> UI e a
createUI createComp id default = UI $ do
  chan <- channel default
  comp <- createComp id default (send chan)
  let signal = subscribe chan
  return $ Flare [comp] signal

-- | Creates a text field for a `Number` input from a given label and default
-- | value.
number :: forall e. Label -> Number -> UI e Number
number = createUI cNumber

-- | Creates a text field for a `Number` input with a default value.
number_ :: forall e. Number -> UI e Number
number_ = number ""

-- | Creates a slider for a `Number` input from a given label,
-- | minimum value, maximum value, step size as well as default value.
numberRange :: forall e. Label -> Number -> Number -> Number -> Number -> UI e Number
numberRange id min max step default = createUI (cNumberRange min max step) id default

-- | Creates a slider for a `Number` input without a label.
numberRange_ :: forall e. Number -> Number -> Number -> Number -> UI e Number
numberRange_ = numberRange ""

-- | Creates a text field for an `Int` input from a given label and default
-- | value.
int :: forall e. Label -> Int -> UI e Int
int = createUI cInt

-- | Creates a text field for an `Int` input with a default value.
int_ :: forall e. Int -> UI e Int
int_ = int ""

-- | Creates a slider for an `Int` input from a given label, minimum and
-- | maximum values as well as a default value.
intRange :: forall e. Label -> Int -> Int -> Int -> UI e Int
intRange id min max default = createUI (cIntRange min max) id default

-- | Creates a slider for an `Int` input without a label.
intRange_ :: forall e. Int -> Int -> Int -> UI e Int
intRange_ = intRange ""

-- | Creates a text field for a `String` input from a given label and default
-- | value.
string :: forall e. Label -> String -> UI e String
string = createUI cString

-- | Creates a text field for a `String` input with a default value.
string_ :: forall e. String -> UI e String
string_ = string ""

-- | Creates a checkbox for a `Boolean` input from a given label and default
-- | value.
boolean :: forall e. Label -> Boolean -> UI e Boolean
boolean = createUI cBoolean

-- | Creates a checkbox for a `Boolean` input with a default value.
boolean_ :: forall e. Boolean -> UI e Boolean
boolean_ = boolean ""

-- | Creates a select box to choose from a list of options. The first option
-- | is selected by default. The rest of the options is given as an array.
select :: forall e a. (Show a) => Label -> a -> Array a -> UI e a
select id default xs = createUI (cSelect xs) id default

-- | Create a select box without a label.
select_ :: forall e a. (Show a) => a -> Array a -> UI e a
select_ = select ""

-- | Attach all elements in the array to the specified parent element.
appendComponents :: forall e. ElementId
                 -> Array Element
                 -> Eff (dom :: DOM | e) Unit
appendComponents = traverse_ <<< appendComponent

-- | Renders a Flare UI to the DOM and sets up all event handlers. The two IDs
-- | specify the DOM elements to which the controls and the output will be
-- | attached, respectively.
runFlare :: forall e a. (Show a)
         => ElementId
         -> ElementId
         -> UI e a
         -> Eff (dom :: DOM, chan :: Chan | e) Unit
runFlare controls target (UI setupUI) = do
  (Flare components signal) <- setupUI
  appendComponents controls components
  runSignal (signal ~> show >>> renderString target)
