## Module Flare

#### `lift`

``` purescript
lift :: forall a. Signal a -> Flare a
```

Lift a `Signal` to a `Flare`.

#### `number`

``` purescript
number :: Label -> Number -> Flare Number
```

#### `number_`

``` purescript
number_ :: Number -> Flare Number
```

Creates a text field for a `Number` input with a default value.

#### `numberRange`

``` purescript
numberRange :: Label -> Number -> Number -> Number -> Number -> Flare Number
```

Creates a slider for a `Number` input from a given label,
minimum value, maximum value, step size as well as default value.

#### `numberRange_`

``` purescript
numberRange_ :: Number -> Number -> Number -> Number -> Flare Number
```

Creates a slider for a `Number` input without a label.

#### `int`

``` purescript
int :: Label -> Int -> Flare Int
```

Creates a text field for an `Int` input from a given label and default
value.

#### `int_`

``` purescript
int_ :: Int -> Flare Int
```

Creates a text field for an `Int` input with a default value.

#### `intRange`

``` purescript
intRange :: Label -> Int -> Int -> Int -> Flare Int
```

Creates a slider for an `Int` input from a given label, minimum and
maximum values as well as a default value.

#### `intRange_`

``` purescript
intRange_ :: Int -> Int -> Int -> Flare Int
```

Creates a slider for an `Int` input without a label.

#### `string`

``` purescript
string :: Label -> String -> Flare String
```

Creates a text field for a `String` input from a given label and default
value.

#### `string_`

``` purescript
string_ :: String -> Flare String
```

Creates a text field for a `String` input with a default value.

#### `boolean`

``` purescript
boolean :: Label -> Boolean -> Flare Boolean
```

Creates a checkbox for a `Boolean` input from a given label and default
value.

#### `boolean_`

``` purescript
boolean_ :: Boolean -> Flare Boolean
```

Creates a checkbox for a `Boolean` input with a default value.

#### `button`

``` purescript
button :: Label -> Flare Boolean
```

Creates a button which yields `true` if is pressed and `false` otherwise.

#### `runFlare`

``` purescript
runFlare :: forall e a. (Show a) => ElementId -> ElementId -> Flare a -> Eff (dom :: DOM, chan :: Chan | e) Unit
```


