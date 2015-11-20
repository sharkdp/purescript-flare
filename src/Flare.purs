module Flare
  ( UI()
  , number
  , int
  , string
  , boolean
  , runFlare
  , module Control.Monad.Eff
  , module DOM
  ) where

import Prelude

import Control.Apply
import Control.Monad.Eff

import DOM

import Signal

type ElementId = String

-- | The main data type for a Flare UI. It encapsulates the `Eff` actions
-- | which are to be run when setting up the input elements and corresponding
-- | signals.
data UI a = UI (ElementId -> Eff (dom :: DOM) (Signal a))

instance functorUI :: Functor UI where
  map f (UI a) = UI (\id -> a id >>= map f >>> pure)

instance applyUI :: Apply UI where
  apply (UI a1) (UI a2) = UI (\id -> lift2 apply (a1 id) (a2 id))

instance applicativeUI :: Applicative UI where
  pure x = UI (const (pure (pure x)))

foreign import render :: forall e. ElementId
                      -> String
                      -> Eff (dom :: DOM | e) Unit

type InputSignal a = forall e c. (c -> Signal c)
                     -> ElementId
                     -> a
                     -> (ElementId -> Eff (dom :: DOM | e) (Signal a))

foreign import iNumber :: InputSignal Number
foreign import iInt :: InputSignal Int
foreign import iString :: InputSignal String
foreign import iBoolean :: InputSignal Boolean

-- | Creates a text field for a `Number` input from a given label and default
-- | value.
number :: ElementId -> Number -> UI Number
number id default = UI (iNumber constant id default)

-- | Creates a text field for an `Int` input from a given label and default
-- | value.
int :: ElementId -> Int -> UI Int
int id default = UI (iInt constant id default)

-- | Creates a text field for a `String` input from a given label and default
-- | value.
string :: ElementId -> String -> UI String
string id default = UI (iString constant id default)

-- | Creates a checkbox for a `Boolean` input from a given label and default
-- | value.
boolean :: ElementId -> Boolean -> UI Boolean
boolean id default = UI (iBoolean constant id default)

-- | Create the Flare UI and run the corresponding signals.
runFlare :: forall a. (Show a)
         => ElementId
         -> ElementId
         -> UI a
         -> Eff (dom :: DOM) Unit
runFlare controls target (UI setupUI) = do
    sig <- setupUI controls
    runSignal (sig ~> show >>> render target)
