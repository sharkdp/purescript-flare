module Flare
  ( Flare(..)
  , UI(..)
  , ElementId()
  , wrap
  , lift
  , foldp
  , number
  , number_
  , numberRange
  , numberRange_
  , numberSlider
  , numberSlider_
  , int
  , int_
  , intRange
  , intRange_
  , intSlider
  , intSlider_
  , string
  , string_
  , boolean
  , boolean_
  , button
  , select
  , select_
  , runFlareWith
  , runFlare
  , runFlareS
  ) where

import Prelude

import Data.Monoid
import Data.Foldable (traverse_)

import Control.Apply
import Control.Monad.Eff

import DOM
import DOM.Node.Types (Element())

import qualified Signal as S
import Signal.Channel

type ElementId = String
type Label = String

-- | A `Flare` is a `Signal` with a corresponding list of HTML elements
-- | for the user interface components.
data Flare a = Flare (Array Element) (S.Signal a)

instance functorFlare :: Functor Flare where
  map f (Flare cs sig) = Flare cs (map f sig)

instance applyFlare :: Apply Flare where
  apply (Flare cs1 sig1) (Flare cs2 sig2) = Flare (cs1 <> cs2) (sig1 <*> sig2)

instance applicativeFlare :: Applicative Flare where
  pure x = Flare [] (pure x)

-- | The main data type for a Flare UI. It encapsulates the `Eff` action
-- | which is to be run when setting up the input elements and corresponding
-- | signals.
newtype UI e a = UI (Eff (dom :: DOM, chan :: Chan | e) (Flare a))

instance functorUI :: Functor (UI e) where
  map f (UI a) = UI $ map (map f) a

instance applyUI :: Apply (UI e) where
  apply (UI a1) (UI a2) = UI $ lift2 apply a1 a2

instance applicativeUI :: Applicative (UI e) where
  pure x = UI $ return (pure x)

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

-- | Encapsulate a `Signal` within a `UI` component.
wrap :: forall e a. (S.Signal a) -> UI e a
wrap sig = UI $ return $ Flare [] sig

-- | Lift a `Signal` inside the `Eff` monad to a `UI` component.
lift :: forall e a. Eff (chan :: Chan, dom :: DOM | e) (S.Signal a) -> UI e a
lift msig = UI $ do
  sig <- msig
  return $ Flare [] sig

-- | Create a past dependent component. The fold-function takes the current
-- | value of the component and the previous value of the output to produce
-- | the new value of the output.
foldp :: forall a b e. (a -> b -> b) -> b -> UI e a -> UI e b
foldp f x0 (UI setup) = UI $ do
  (Flare comp sig) <- setup
  return $ Flare comp (S.foldp f x0 sig)

-- | Remove all children from a given parent element.
foreign import removeChildren :: forall e. ElementId
                              -> Eff (dom :: DOM | e) Unit

-- | Append a child element to the parent with the specified ID.
foreign import appendComponent :: forall e. ElementId
                               -> Element -> Eff (dom :: DOM | e) Unit

-- | Set the inner HTML of the specified element to the given value.
foreign import renderString :: forall e. ElementId
                            -> String
                            -> Eff (dom :: DOM | e) Unit

type CreateComponent a = forall e. Label
                         -> a
                         -> (a -> Eff (chan :: Chan) Unit)
                         -> Eff (dom :: DOM, chan :: Chan | e) Element

foreign import cNumber :: CreateComponent Number
foreign import cNumberRange :: String -> Number -> Number -> Number -> CreateComponent Number
foreign import cIntRange :: String -> Int -> Int -> CreateComponent Int
foreign import cString :: CreateComponent String
foreign import cBoolean :: CreateComponent Boolean
foreign import cButton :: CreateComponent Boolean
foreign import cSelect :: forall a. (Show a) => Array a -> CreateComponent a

-- | Set up the HTML element for a given component and create the corresponding
-- | signal channel.
createUI :: forall e a. (CreateComponent a) -> Label -> a -> UI e a
createUI createComp id default = UI $ do
  chan <- channel default
  comp <- createComp id default (send chan)
  let signal = subscribe chan
  return $ Flare [comp] signal

-- | Creates an input field for a `Number` from a given label and default
-- | value.
number :: forall e. Label -> Number -> UI e Number
number = createUI cNumber

-- | Like `number`, but without a label.
number_ :: forall e. Number -> UI e Number
number_ = number ""

-- | Creates an input field for a `Number` from a given label,
-- | minimum value, maximum value, step size as well as default value.
-- | The returned value is guaranteed to be within the given range.
numberRange :: forall e. Label -> Number -> Number -> Number -> Number -> UI e Number
numberRange id min max step default = createUI (cNumberRange "number" min max step) id default

-- | Like `numberRange`, but without a label.
numberRange_ :: forall e. Number -> Number -> Number -> Number -> UI e Number
numberRange_ = numberRange ""

-- | Creates a slider for a `Number` input from a given label,
-- | minimum value, maximum value, step size as well as default value.
numberSlider :: forall e. Label -> Number -> Number -> Number -> Number -> UI e Number
numberSlider id min max step default = createUI (cNumberRange "range" min max step) id default

-- | Like `numberSlider`, but without a label.
numberSlider_ :: forall e. Number -> Number -> Number -> Number -> UI e Number
numberSlider_ = numberSlider ""

-- | Creates an input field for an `Int` from a given label and default
-- | value. The returned value is guaranteed to be within the allowed integer
-- | range.
int :: forall e. Label -> Int -> UI e Int
int id = createUI (cIntRange "number" bottom top) id

-- | Like `int`, but without a label.
int_ :: forall e. Int -> UI e Int
int_ = int ""

-- | Creates an input field for an `Int` from a given label, minimum and
-- | maximum values as well as a default value. The returned value is
-- | guaranteed to be within the given range.
intRange :: forall e. Label -> Int -> Int -> Int -> UI e Int
intRange id min max default = createUI (cIntRange "number" min max) id default

-- | Like `intRange`, but without a label.
intRange_ :: forall e. Int -> Int -> Int -> UI e Int
intRange_ = intRange ""

-- | Creates a slider for an `Int` input from a given label, minimum and
-- | maximum values as well as a default value.
intSlider :: forall e. Label -> Int -> Int -> Int -> UI e Int
intSlider id min max default = createUI (cIntRange "range" min max) id default

-- | Like `intSlider`, but without a label.
intSlider_ :: forall e. Int -> Int -> Int -> UI e Int
intSlider_ = intSlider ""

-- | Creates a text field for a `String` input from a given label and default
-- | value.
string :: forall e. Label -> String -> UI e String
string = createUI cString

-- | Like `string`, but without a label.
string_ :: forall e. String -> UI e String
string_ = string ""

-- | Creates a checkbox for a `Boolean` input from a given label and default
-- | value.
boolean :: forall e. Label -> Boolean -> UI e Boolean
boolean = createUI cBoolean

-- | Like `boolean`, but without a label.
boolean_ :: forall e. Boolean -> UI e Boolean
boolean_ = boolean ""

-- | Creates a button which yields `true` if is pressed and `false` otherwise.
button :: forall e. Label -> UI e Boolean
button id = createUI cButton id false

-- | Creates a select box to choose from a list of options. The first option
-- | is selected by default. The rest of the options is given as an array.
select :: forall e a. (Show a) => Label -> a -> Array a -> UI e a
select id default xs = createUI (cSelect xs) id default

-- | Like `select`, but without a label.
select_ :: forall e a. (Show a) => a -> Array a -> UI e a
select_ = select ""

-- | Renders a Flare UI to the DOM and sets up all event handlers. The ID
-- | specifies the HTML element to which the controls are attached. The
-- | function argument will be mapped over the `Signal` inside the `Flare`.
runFlareWith :: forall e a. ElementId
             -> (a -> Eff (dom :: DOM, chan :: Chan | e) Unit)
             -> UI e a
             -> Eff (dom :: DOM, chan :: Chan | e) Unit
runFlareWith controls handler (UI setupUI) = do
  (Flare components signal) <- setupUI
  removeChildren controls
  traverse_ (appendComponent controls) components
  S.runSignal (map handler signal)

-- | Renders a Flare UI to the DOM and sets up all event handlers. The two IDs
-- | specify the DOM elements to which the controls and the output will be
-- | attached, respectively.
runFlare :: forall e a. (Show a)
         => ElementId
         -> ElementId
         -> UI e a
         -> Eff (dom :: DOM, chan :: Chan | e) Unit
runFlare controls target =
  runFlareWith controls (show >>> renderString target)

-- | Like `runFlare`, but does not run `show` on the `String`, so as to prevent
-- | the double quotes around the string.
runFlareS :: forall e.
             ElementId
          -> ElementId
          -> UI e String
          -> Eff (dom :: DOM, chan :: Chan | e) Unit
runFlareS controls target = runFlareWith controls (renderString target)
