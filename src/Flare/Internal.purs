module Flare.Internal
  ( UI(..)
  , SetupUI(..)
  , CreateComponent(..)
  , UpdateHandler(..)
  , appendComponent
  , renderString
  , cellToUI
  , cNumber
  , cNumberRange
  , cInt
  , cIntRange
  , cString
  , cBoolean
  , cButton
  , cSelect
  , makeFlare
  , runFlareWith
  ) where

import Prelude

import Data.Array (reverse)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)

import Control.Apply
import Control.Monad.Eff
import Control.Applicative.Free

import DOM
import DOM.Node.Types (Element())

import qualified Signal as S
import Signal.Channel

import Flare.Types

-- | A `UI` is a `Signal` with a collection of HTML elements for the controls.
data UI a = UI (Array Element) (S.Signal a)

instance functorUI :: Functor UI where
  map f (UI cs sig) = UI cs (map f sig)

instance applyUI :: Apply UI where
  apply (UI cs1 sig1) (UI cs2 sig2) = UI (cs1 <> cs2) (sig1 <*> sig2)

instance applicativeUI :: Applicative UI where
  pure x = UI [] (pure x)

-- | A wrapper around `UI`.
newtype SetupUI e a = SetupUI (Eff (dom :: DOM, chan :: Chan | e) (UI a))

instance functorSetupUI :: Functor (SetupUI e) where
  map f (SetupUI a) = SetupUI $ map (map f) a

instance applySetupUI :: Apply (SetupUI e) where
  apply (SetupUI a1) (SetupUI a2) = SetupUI $ lift2 apply a1 a2

instance applicativeSetupUI :: Applicative (SetupUI e) where
  pure x = SetupUI $ return (pure x)

-- | A function which sends the current value of the component into the
-- | corresponding `Signal.Channel`.
type UpdateHandler a = a -> Eff (chan :: Chan) Unit

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

-- | Lift a component and the corresponding value into the free `Flare` functor.
makeFlare :: forall a. Component -> a -> Flare a
makeFlare component default = Flare (liftFreeAp (Cell [component] default))

-- | Append a child element to the parent with the specified ID
foreign import appendComponent :: forall e. ElementId
                               -> Element -> Eff (dom :: DOM | e) Unit

-- | Set the inner HTML of the specified element to the given value
foreign import renderString :: forall e. ElementId
                            -> String
                            -> Eff (dom :: DOM | e) Unit

-- | Convert a `Component` to an actual HTML element.
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

-- | Transforms a`Cell` to a corresponding `UI`. This is an effectful operation
-- | which sets up a `Signal.Channel` and creates the HTML elements for the
-- | controls, with corresponding event handlers.
cellToUI :: forall e. NaturalTransformation Cell (SetupUI e)
cellToUI (Cell components x) = SetupUI $ do
  chan <- channel x
  elements <- traverse (toElement (send chan)) components
  return (UI elements (subscribe chan))
cellToUI (Lift sig) = SetupUI $ return (UI [] sig)

-- | Renders a `Flare` to the DOM and sets up all event handlers. The ID
-- | specifies the HTML element to which the controls are attached.
runFlareWith :: forall e a. ElementId
             -> (a -> Eff (dom :: DOM, chan :: Chan | e) Unit)
             -> Flare a
             -> Eff (dom :: DOM, chan :: Chan | e) Unit
runFlareWith controls action (Flare flare) =
  case foldFreeAp cellToUI flare of
    (SetupUI setup) -> do
      (UI els sig) <- setup
      traverse_ (appendComponent controls) (reverse els)

      S.runSignal (map action sig)
