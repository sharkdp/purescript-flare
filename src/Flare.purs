module Flare
  ( Flare()
  , UI()
  , number
  , int
  , string
  , boolean
  , runFlare
  , module Control.Monad.Eff
  , module DOM
  , module Signal.Channel
  ) where

import Prelude

import Data.Foldable (traverse_)

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
newtype UI a = UI (Eff (dom :: DOM, chan :: Chan) (Flare a))

instance functorUI :: Functor UI where
  map f (UI a) = UI $ do
    (Flare cs sig) <- a
    return $ Flare cs (map f sig)

instance applyUI :: Apply UI where
  apply (UI a1) (UI a2) = UI $ do
    (Flare cs1 sig1) <- a1
    (Flare cs2 sig2) <- a2
    return $ Flare (cs1 <> cs2) (apply sig1 sig2)

instance applicativeUI :: Applicative UI where
  pure x = UI $ return (Flare [] (pure x))

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

-- | Set up the HTML element for a given component and create the corresponding
-- | signal channel.
createUI :: forall a. (CreateComponent a) -> Label -> a -> UI a
createUI createComp id default = UI $ do
  chan <- channel default
  comp <- createComp id default (send chan)
  let signal = subscribe chan
  return $ Flare [comp] signal

-- | Creates a text field for a `Number` input from a given label and default
-- | value.
number :: Label -> Number -> UI Number
number = createUI cNumber

-- | Creates a text field for an `Int` input from a given label and default
-- | value.
int :: Label -> Int -> UI Int
int = createUI cInt

-- | Creates a text field for a `String` input from a given label and default
-- | value.
string :: Label -> String -> UI String
string = createUI cString

-- | Creates a checkbox for a `Boolean` input from a given label and default
-- | value.
boolean :: Label -> Boolean -> UI Boolean
boolean = createUI cBoolean

-- | Render a Flare UI to the DOM and set up all event handlers.
runFlare :: forall a. (Show a)
         => ElementId
         -> ElementId
         -> UI a
         -> Eff (dom :: DOM, chan :: Chan) Unit
runFlare controls target (UI setupUI) = do
  (Flare components signal) <- setupUI
  traverse_ (appendComponent controls) components
  runSignal (signal ~> show >>> renderString target)
