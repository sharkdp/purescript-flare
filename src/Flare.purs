module Flare
  ( lift
  , foldp
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
  , runFlare
  , module Flare.Types
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Applicative.Free (liftFreeAp)

import DOM (DOM())

import Signal hiding (foldp)
import Signal.Channel (Chan())

import Flare.Types
import Flare.Internal

-- | Lift a `Signal` to a `Flare`.
lift :: forall a. Signal a -> Flare a
lift sig = Flare (liftFreeAp (Lift sig))

foreign import foldp_ :: forall a b. (a -> b -> b) -> b -> (a -> b)

-- | Create a past dependent component. The fold-function takes the current
-- | value of the component and the previous value of the output to produce
-- | the new value of the output.
foldp :: forall a b. (a -> b -> b) -> b -> Flare a -> Flare b
foldp f s0 = map (foldp_ f s0)

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

-- | Creates a text field for a `String` input with a default value.
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

-- | Renders a `Flare` to the DOM and sets up all event handlers. The two IDs
-- | specify the DOM elements to which the controls and the output will be
-- | attached, respectively.
runFlare :: forall e a. (Show a)
         => ElementId
         -> ElementId
         -> Flare a
         -> Eff (dom :: DOM, chan :: Chan | e) Unit
runFlare controls target =
  runFlareWith controls (show >>> renderString target)
