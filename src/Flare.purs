module Flare
  ( Flare()
  , UI()
  , ElementId()
  , Label()
  , number
  , number_
  , numberRange
  , numberRange_
  , numberSlider
  , numberSlider_
  , innerFlare
  , int
  , int_
  , intRange
  , intRange_
  , intSlider
  , intSlider_
  , string
  , string_
  , stringPattern
  , stringPattern_
  , boolean
  , boolean_
  , optional
  , optional_
  , button
  , buttons
  , select
  , select_
  , radioGroup
  , radioGroup_
  , textarea
  , textarea_
  , color
  , color_
  , date
  , date_
  , time
  , time_
  , fieldset
  , resizableList
  , resizableList_
  , applyUIFlipped
  , (<**>)
  , wrap
  , lift
  , liftSF
  , foldp
  , foldEffect
  , setupFlare
  , flareWith
  , runFlareWith
  , runFlare
  , runFlareShow
  ) where

import Prelude

import Color (Color, toHexString, fromHexString)
import Control.Apply (lift2)
import Data.Array (fromFoldable)
import Data.Date (Date, exactDate)
import Data.Date as Date
import Data.Enum (toEnum, fromEnum)
import Data.Foldable (class Foldable, traverse_, foldMap)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Time (Time(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Signal as S
import Signal.Channel (subscribe, send, channel)
import Signal.Effect as SE
import Web.DOM (Element)

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

-- | The main data type for a Flare UI. It encapsulates the `Effect` action
-- | which is to be run when setting up the input elements and corresponding
-- | signals.
newtype UI a = UI (Effect (Flare a))

instance functorUI :: Functor UI where
  map f (UI a) = UI $ map (map f) a

instance applyUI :: Apply UI where
  apply (UI a1) (UI a2) = UI $ lift2 apply a1 a2

instance applicativeUI :: Applicative UI where
  pure x = UI $ pure (pure x)

instance semigroupUI :: (Semigroup a) => Semigroup (UI a) where
  append = lift2 append

instance monoidUI :: (Monoid a) => Monoid (UI a) where
  mempty = pure mempty

-- | Remove all children from a given parent element.
foreign import removeChildren :: ElementId -> Effect Unit

-- | Append a child element to the parent with the specified ID.
foreign import appendComponent :: ElementId -> Element -> Effect Unit

foreign import createInnerElementP :: forall a. (ElementId -> Element -> a) -> Effect a

createInnerElement :: Effect (Tuple ElementId Element)
createInnerElement = createInnerElementP Tuple

-- | Set the inner HTML of the specified element to the given value.
foreign import renderString :: ElementId -> String -> Effect Unit

type CreateComponent a = Label
                         -> a
                         -> (a -> Effect Unit)
                         -> Effect Element

foreign import cNumber :: CreateComponent Number
foreign import cNumberRange :: String -> Number -> Number -> Number -> CreateComponent Number
foreign import cIntRange :: String -> Int -> Int -> CreateComponent Int
foreign import cString :: CreateComponent String
foreign import cStringPattern :: String -> CreateComponent String
foreign import cBoolean :: CreateComponent Boolean
foreign import cButton :: forall a. a -> CreateComponent a
foreign import cSelect :: forall a. Array a -> (a -> String) -> CreateComponent a
foreign import cRadioGroup :: forall a. Array a -> (a -> String) -> CreateComponent a
foreign import cTextarea :: CreateComponent String
foreign import cColor :: CreateComponent String

type DateRec = { year :: Int, month :: Int, day :: Int }
type TimeRec = { hours :: Int, minutes :: Int }

foreign import cDate :: CreateComponent DateRec
foreign import cTime :: CreateComponent TimeRec

-- | Set up the HTML element for a given component and create the corresponding
-- | signal channel.
createUI :: forall a. (CreateComponent a) -> Label -> a -> UI a
createUI createComp label default = UI $ do
  chan <- channel default
  comp <- createComp label default (send chan)
  let signal = subscribe chan
  pure $ Flare [comp] signal

-- | Creates an input field for a `Number` from a given label and default
-- | value.
number :: Label -> Number -> UI Number
number = createUI cNumber

-- | Like `number`, but without a label.
number_ :: Number -> UI Number
number_ = number ""

-- | Creates an input field for a `Number` from a given label,
-- | minimum value, maximum value, step size as well as default value.
-- | The returned value is guaranteed to be within the given range.
numberRange :: Label -> Number -> Number -> Number -> Number -> UI Number
numberRange label min max step default = createUI (cNumberRange "number" min max step) label default

-- | Like `numberRange`, but without a label.
numberRange_ :: Number -> Number -> Number -> Number -> UI Number
numberRange_ = numberRange ""

-- | Creates a slider for a `Number` input from a given label,
-- | minimum value, maximum value, step size as well as default value.
numberSlider :: Label -> Number -> Number -> Number -> Number -> UI Number
numberSlider label min max step default = createUI (cNumberRange "range" min max step) label default

-- | Like `numberSlider`, but without a label.
numberSlider_ :: Number -> Number -> Number -> Number -> UI Number
numberSlider_ = numberSlider ""

-- | Creates an input field for an `Int` from a given label and default
-- | value. The returned value is guaranteed to be within the allowed integer
-- | range.
int :: Label -> Int -> UI Int
int label = createUI (cIntRange "number" bottom top) label

-- | Like `int`, but without a label.
int_ :: Int -> UI Int
int_ = int ""

-- | Creates an input field for an `Int` from a given label, minimum and
-- | maximum values as well as a default value. The returned value is
-- | guaranteed to be within the given range.
intRange :: Label -> Int -> Int -> Int -> UI Int
intRange label min max default = createUI (cIntRange "number" min max) label default

-- | Like `intRange`, but without a label.
intRange_ :: Int -> Int -> Int -> UI Int
intRange_ = intRange ""

-- | Creates a slider for an `Int` input from a given label, minimum and
-- | maximum values as well as a default value.
intSlider :: Label -> Int -> Int -> Int -> UI Int
intSlider label min max default = createUI (cIntRange "range" min max) label default

-- | Like `intSlider`, but without a label.
intSlider_ :: Int -> Int -> Int -> UI Int
intSlider_ = intSlider ""

-- | Creates a text field for a `String` input from a given label and default
-- | value.
string :: Label -> String -> UI String
string = createUI cString

-- | Like `string`, but without a label.
string_ :: String -> UI String
string_ = string ""

-- | Creates a text field for a `String` input from a given label, validation
-- | pattern (HTML5 `pattern` attribute), and a default value.
stringPattern :: Label -> String -> String -> UI String
stringPattern label pattern default = createUI (cStringPattern pattern) label default

-- | Like `stringPattern`, but without a label.
stringPattern_ :: String -> String -> UI String
stringPattern_ = stringPattern ""

-- | Creates a checkbox for a `Boolean` input from a given label and default
-- | value.
boolean :: Label -> Boolean -> UI Boolean
boolean = createUI cBoolean

-- | Like `boolean`, but without a label.
boolean_ :: Boolean -> UI Boolean
boolean_ = boolean ""

-- | Creates a checkbox that returns `Just x` if enabled and `Nothing` if
-- | disabled. Takes a label, the initial state (enabled or disabled) and
-- | the default value `x`.
optional :: forall a. Label -> Boolean -> a -> UI (Maybe a)
optional label enabled x = ret <$> boolean label enabled
  where ret true = (Just x)
        ret false = Nothing

-- | Like `optional`, but without a label.
optional_ :: forall a. Boolean -> a -> UI (Maybe a)
optional_ = optional ""

-- | Creates a button which yields the first value in the default state and
-- | the second value when it is pressed.
button :: forall a. Label -> a -> a -> UI a
button label vDefault vPressed = createUI (cButton vPressed) label vDefault

-- | Create a button for each element of the given container. The whole
-- | component returns `Nothing` if none of the buttons is pressed and `Just x`
-- | if the button corresponding to the element `x` is pressed.
buttons :: forall f a. Traversable f => f a -> (a -> String) -> UI (Maybe a)
buttons xs toString =  (unwrap <<< foldMap First) <$> traverse toButton xs
  where
    toButton :: a -> UI (Maybe a)
    toButton x = button (toString x) Nothing (Just x)

-- | Creates a select box to choose from a list of options. The first option
-- | is selected by default. The rest of the options is given as an array.
select :: forall f a. Foldable f => Label -> NonEmpty f a -> (a -> String) -> UI a
select label (default :| xs) toString =
  createUI (cSelect (fromFoldable xs) toString) label default

-- | Like `select`, but without a label.
select_ :: forall f a. Foldable f => NonEmpty f a -> (a -> String) -> UI a
select_ = select ""

-- | Creates a group of radio buttons to choose from a list of options. The
-- | first option is selected by default. The rest of the options is given as
-- | an array.
radioGroup :: forall f a. Foldable f => Label -> NonEmpty f a -> (a -> String) -> UI a
radioGroup label (default :| xs) toString =
  createUI (cRadioGroup (fromFoldable xs) toString) label default

-- | Like `radioGroup`, but without a label.
radioGroup_ :: forall f a. Foldable f => NonEmpty f a -> (a -> String) -> UI a
radioGroup_ = radioGroup ""

-- | Creates a textarea field for a `String` input from a given label and
-- | default value.
textarea :: Label -> String -> UI String
textarea = createUI cTextarea

-- | Like `textarea`, but without a label.
textarea_ :: String -> UI String
textarea_ = textarea ""

-- | Creates a color picker input field from a label and default `Color`.
color :: Label -> Color -> UI Color
color label default = (fromMaybe default <<< fromHexString) <$>
                        createUI cColor label (toHexString default)

-- | Like `color`, but without a label.
color_ :: Color -> UI Color
color_ = color ""

-- | Creates a date input field from a label and default `Date`.
date :: Label -> Date -> UI Date
date label default = (fromMaybe default <<< toDate) <$>
                        createUI cDate label { year: fromEnum (Date.year default)
                                             , month: fromEnum (Date.month default)
                                             , day: fromEnum (Date.day default)
                                             }
  where
    toDate :: DateRec -> Maybe Date
    toDate { year, month, day } = do
      y <- toEnum year
      m <- toEnum month
      d <- toEnum day
      exactDate y m d

-- | Like `date`, but without a label.
date_ :: Date -> UI Date
date_ = date ""

-- | Creates a time input field from a label and default `Time`.
time :: Label -> Time -> UI Time
time label default = (fromMaybe default <<< toTime) <$>
                       createUI cTime label { hours: 0, minutes: 30 }
  where
    toTime :: TimeRec -> Maybe Time
    toTime { hours, minutes } = Time <$> toEnum hours
                                     <*> toEnum minutes
                                     <*> toEnum 0
                                     <*> toEnum 0

-- | Like `time`, but without a label.
time_ :: Time -> UI Time
time_ = time ""

foreign import toFieldset :: Label -> Array Element -> Element

-- | Group the components of a UI inside a fieldset element with a given title.
fieldset :: forall a. Label -> UI a -> UI a
fieldset label (UI setup) = UI $ do
  (Flare cs sig) <- setup
  pure $ Flare [toFieldset label cs] sig

foreign import cResizableList :: forall a. (List a -> List a)
                              -> (List a -> UI (List a))
                              -> CreateComponent (List a)

-- | Creates a resizable `List a` input given a way to construct `a` UIs, a
-- | default `a`, and a default `List a`.
resizableList :: forall a. Label ->
                 (a -> UI a) ->
                 a ->
                 List a ->
                 UI (List a)
resizableList label aUi defaultA defaultList =
  createUI (cResizableList prependDefault listUi) label defaultList where
    prependDefault = Cons defaultA
    -- a UI missing +/- buttons at the top (which `cResizableList` then adds)
    listUi = case _ of
      Nil -> pure Nil
      hd : tl -> Cons <$> aUi hd
                      <*> resizableList "" aUi defaultA tl

-- | Like `resizableList`, but without a label.
resizableList_ :: forall a. (a -> UI a) -> a -> List a -> UI (List a)
resizableList_ = resizableList ""

-- | A flipped version of `<*>` for `UI` that arranges the components in the
-- | order of appearance.
applyUIFlipped :: forall a b. UI a -> UI (a -> b) -> UI b
applyUIFlipped (UI setup1) (UI setup2) = UI $ do
  (Flare cs1 sig1) <- setup1
  (Flare cs2 sig2) <- setup2
  pure $ Flare (cs1 <> cs2) (sig2 <*> sig1)

infixl 4 applyUIFlipped as <**>

-- | Encapsulate a `Signal` within a `UI` component.
wrap :: forall a. (S.Signal a) -> UI a
wrap sig = UI $ pure $ Flare [] sig

-- | Lift a `Signal` inside the `Eff` monad to a `UI` component.
lift :: forall a. Effect (S.Signal a) -> UI a
lift msig = UI $ do
  sig <- msig
  pure $ Flare [] sig

-- | Lift a function from `Signal a` to `Signal b` to a function from
-- | `UI a` to `UI b` without affecting the components. For example:
-- |
-- | ``` purescript
-- | dropRepeats :: forall a. (Eq a) => UI a -> UI a
-- | dropRepeats = liftSF S.dropRepeats
-- | ```
liftSF :: forall a b. (S.Signal a -> S.Signal b)
       -> UI a
       -> UI b
liftSF f (UI setup) = UI do
  (Flare comp sig) <- setup
  pure $ Flare comp (f sig)

-- | Create a past dependent component. The fold-function takes the current
-- | value of the component and the previous value of the output to produce
-- | the new value of the output.
foldp :: forall a b. (a -> b -> b) -> b -> UI a -> UI b
foldp f x0 = liftSF (S.foldp f x0)

-- | Creates a past dependent component with an effectful computation.
-- | The function argument takes the value of the input component, and
-- | the previous value of the output component, to produce the new value
-- | of the output component wrapped inside an Effect action.
foldEffect :: forall a b. (a -> b -> Effect b) -> b -> UI a -> UI b
foldEffect f b (UI eff) = lift $ do
    (Flare _ signal) <- eff
    SE.foldEffect f b signal

-- | Low level function to get direct access to the HTML elements and the
-- | `Signal` inside a Flare UI.
setupFlare :: forall a. UI a
            -> Effect { components :: Array Element
                      , signal :: S.Signal a }
setupFlare (UI setupUI) = do
  (Flare components signal) <- setupUI
  pure { components, signal }

-- | Renders a Flare UI to the DOM and sets up all event handlers. The ID
-- | specifies the HTML element to which the controls are attached. The
-- | handler function argument handles the `Signal` inside the `Flare`.
flareWith :: forall a. ElementId
          -> (S.Signal a -> Effect Unit)
          -> UI a
          -> Effect Unit
flareWith controls handler (UI setupUI) = do
  (Flare components signal) <- setupUI
  removeChildren controls
  traverse_ (appendComponent controls) components
  handler signal

-- | Renders an UI with access to the current value.
innerFlare :: forall a b. UI a
           -> (a -> UI b)
           -> UI b
innerFlare (UI setupUI) innerUI = UI $ do
  (Flare components signal) <- setupUI
  -- Get the initial value for the resulting signal
  UI setupInnerUI <- innerUI <$> S.get signal
  (Flare _ innerSignal) <- setupInnerUI
  initialInner <- S.get innerSignal
  innerResult <- channel initialInner
  -- Set up the inner UI
  Tuple innerHostId innerHost <- createInnerElement
  let setupInner = innerUI >>> flareWith innerHostId (map (send innerResult) >>> S.runSignal)
  S.runSignal $ setupInner <$> signal
  pure $ Flare (components <> [innerHost]) (subscribe innerResult)

-- | Renders a Flare UI to the DOM and sets up all event handlers. The ID
-- | specifies the HTML element to which the controls are attached. The
-- | function argument will be mapped over the `Signal` inside the `Flare`.
runFlareWith :: forall a. ElementId
             -> (a -> Effect Unit)
             -> UI a
             -> Effect Unit
runFlareWith controls handler ui = flareWith controls (S.runSignal <<< map handler) ui

-- | Renders a Flare UI to the DOM and sets up all event handlers. The two IDs
-- | specify the DOM elements to which the controls and the output will be
-- | attached, respectively.
runFlare :: ElementId
         -> ElementId
         -> UI String
         -> Effect Unit
runFlare controls target = runFlareWith controls (renderString target)

-- | Like `runFlare` but uses `show` to convert the contained value to a
-- | `String` before rendering to the DOM (useful for testing).
runFlareShow :: forall a. (Show a)
             => ElementId
             -> ElementId
             -> UI a
             -> Effect Unit
runFlareShow controls target ui = runFlare controls target (show <$> ui)
