## Module Flare.Types

#### `Label`

``` purescript
type Label = String
```

A text label for control elements

#### `ElementId`

``` purescript
type ElementId = String
```

The ID of a DOM element

#### `Component`

``` purescript
data Component
  = CNumber Label Number
  | CNumberRange Label Number Number Number Number
  | CInt Label Int
  | CIntRange Label Int Int Int
  | CString Label String
  | CBoolean Label Boolean
  | CButton Label
```

Possible input types.

#### `Cell`

``` purescript
data Cell a
  = Cell (Array Component) a
  | Lift (Signal a)
```

Intermediate data type.

#### `Flare`

``` purescript
data Flare a
  = Flare (FreeAp Cell a)
```

The main data type for a Flare UI.

##### Instances
``` purescript
Functor Flare
Apply Flare
Applicative Flare
(Semigroup a) => Semigroup (Flare a)
(Monoid a) => Monoid (Flare a)
(Semiring a) => Semiring (Flare a)
(Ring a) => Ring (Flare a)
(ModuloSemiring a) => ModuloSemiring (Flare a)
(DivisionRing a) => DivisionRing (Flare a)
(Num a) => Num (Flare a)
(Bounded a) => Bounded (Flare a)
(BooleanAlgebra a) => BooleanAlgebra (Flare a)
```


