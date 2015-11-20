## Flare

Flare is a special-purpose UI library for
[Purescript](https://github.com/purescript/purescript). It is built on top
of [purescript-signal](https://github.com/bodil/purescript-signal) and uses
Applicative style to combine predefined input fields to a reactive user
interface. Flare is inspired by the Haskell library
[typed-spreadsheet](https://github.com/Gabriel439/Haskell-Typed-Spreadsheet-Library).

The main criterion in the design of Flare is ease of use

- [Module documentation](docs/Flare.md)

## Building
```
bower install
pulp build
pulp test -r cat > html/main.js
```

## Example

``` purescript
module Main where

import Prelude
import Flare
import Math (pow)

main = runFlare "controls" "target" $
         pow <$> number "Base" 2.0
             <*> number "Exponent" 10.0
```
