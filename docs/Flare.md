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
number :: ElementId -> Number -> UI Number
```

Creates a text field for a `Number` input from a given label and default
value.

#### `int`

``` purescript
int :: ElementId -> Int -> UI Int
```

Creates a text field for an `Int` input from a given label and default
value.

#### `string`

``` purescript
string :: ElementId -> String -> UI String
```

Creates a text field for a `String` input from a given label and default
value.

#### `boolean`

``` purescript
boolean :: ElementId -> Boolean -> UI Boolean
```

Creates a checkbox for a `Boolean` input from a given label and default
value.

#### `runFlare`

``` purescript
runFlare :: forall a. (Show a) => ElementId -> ElementId -> UI a -> Eff (dom :: DOM) Unit
```

Create the Flare UI and run the corresponding signals.


