## Module Flare

#### `UI`

``` purescript
data UI a
```

The main data type for a Flare UI. It encapsulates the `Eff` actions
which are to be run when setting up the input elements and corresponding
signals.

##### Instances
``` purescript
Functor UI
Apply UI
Applicative UI
```

#### `number`

``` purescript
number :: UIField Number
```

Creates a text field for a `Number` from a given label and default value.

#### `int`

``` purescript
int :: UIField Int
```

Creates a text field for an `Int` from a given label and default value.

#### `string`

``` purescript
string :: UIField String
```

Creates an input field for a `String` from a given label and default value.

#### `boolean`

``` purescript
boolean :: UIField Boolean
```

Creates a checkbox for a `Boolean` value from a given label and default value.

#### `runFlare`

``` purescript
runFlare :: forall a. (Show a) => ElementId -> ElementId -> UI a -> Eff (dom :: DOM) Unit
```

Create the Flare UI and run the corresponding signals.


