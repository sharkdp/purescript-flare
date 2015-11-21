module Flare
  ( Flare(..)
  , UI(..)
  , ElementId()
  , number
  , number_
  , int
  , int_
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
foreign import cInt :: CreateComponent Int
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

-- | Creates a text field for an `Int` input from a given label and default
-- | value.
int :: forall e. Label -> Int -> UI e Int
int = createUI cInt

-- | Creates a text field for an `Int` input with a default value.
int_ :: forall e. Int -> UI e Int
int_ = int ""

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
select :: forall e a. (Show a)
       => Label
       -> a
       -> Array a
       -> UI e a
select id x xs = UI $ do
  chan <- channel x
  comp <- cSelect xs id x (send chan)
  let signal = subscribe chan
  return $ Flare [comp] signal

-- | Create a select box without a label.
select_ :: forall e a. (Show a)
        => a
        -> Array a
        -> UI e a
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
