## Module Flare.Internal

#### `UI`

``` purescript
data UI a
  = UI (Array Element) (Signal a)
```

A `UI` is a `Signal` with a collection of HTML elements for the controls.

##### Instances
``` purescript
Functor UI
Apply UI
Applicative UI
```

#### `SetupUI`

``` purescript
newtype SetupUI e a
  = SetupUI (Eff (dom :: DOM, chan :: Chan | e) (UI a))
```

A wrapper around `UI`.

##### Instances
``` purescript
Functor (SetupUI e)
Apply (SetupUI e)
Applicative (SetupUI e)
```

#### `UpdateHandler`

``` purescript
type UpdateHandler a = a -> Eff (chan :: Chan) Unit
```

A function which sends the current value of the component into the
corresponding `Signal.Channel`.

#### `CreateComponent`

``` purescript
type CreateComponent a = forall a b e. Label -> a -> UpdateHandler b -> Eff (dom :: DOM, chan :: Chan | e) Element
```

#### `cNumber`

``` purescript
cNumber :: CreateComponent Number
```

#### `cNumberRange`

``` purescript
cNumberRange :: Number -> Number -> Number -> CreateComponent Number
```

#### `cInt`

``` purescript
cInt :: CreateComponent Int
```

#### `cIntRange`

``` purescript
cIntRange :: Int -> Int -> CreateComponent Int
```

#### `cString`

``` purescript
cString :: CreateComponent String
```

#### `cBoolean`

``` purescript
cBoolean :: CreateComponent Boolean
```

#### `cButton`

``` purescript
cButton :: CreateComponent Boolean
```

#### `cSelect`

``` purescript
cSelect :: forall a. (Show a) => Array a -> CreateComponent a
```

#### `makeFlare`

``` purescript
makeFlare :: forall a. Component -> a -> Flare a
```

Lift a component and the corresponding value into the free `Flare` functor.

#### `appendComponent`

``` purescript
appendComponent :: forall e. ElementId -> Element -> Eff (dom :: DOM | e) Unit
```

Append a child element to the parent with the specified ID

#### `renderString`

``` purescript
renderString :: forall e. ElementId -> String -> Eff (dom :: DOM | e) Unit
```

Set the inner HTML of the specified element to the given value

#### `cellToUI`

``` purescript
cellToUI :: forall e. NaturalTransformation Cell (SetupUI e)
```

Transforms a`Cell` to a corresponding `UI`. This is an effectful operation
which sets up a `Signal.Channel` and creates the HTML elements for the
controls, with corresponding event handlers.

#### `runFlareWith`

``` purescript
runFlareWith :: forall e a. ElementId -> (a -> Eff (dom :: DOM, chan :: Chan | e) Unit) -> Flare a -> Eff (dom :: DOM, chan :: Chan | e) Unit
```

Renders a `Flare` to the DOM and sets up all event handlers. The ID
specifies the HTML element to which the controls are attached.


