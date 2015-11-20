## Flare

Flare is a special-purpose UI library for
[Purescript](https://github.com/purescript/purescript). It is built on top
of [purescript-signal](https://github.com/bodil/purescript-signal) and uses
Applicative style to combine predefined input fields to a reactive user
interface. Flare is inspired by the Haskell library
[typed-spreadsheet](https://github.com/Gabriel439/Haskell-Typed-Spreadsheet-Library).

The main design-criterion of this library is ease of use.

- [Module documentation](docs/Flare.md)
- [Demo](http://sharkdp.github.io/purescript-flare/)

## Building
```
bower install
pulp build
pulp test -r cat > html/main.js
```

## Example 1

A very basic Flare UI with two input fields:

``` purescript
module Main where

import Prelude
import Flare
import Math (pow)

main = runFlare "controls" "target" $
         pow <$> number "Base" 2.0
             <*> number "Exponent" 10.0
```

Here, `controls` and `target` are IDs of two `<div>` elements in the
corresponding HTML file. The input fields will be appended to `controls` while
the current output will be rendered to the `target` element.

## Example 2

Another simple UI with a text field and a checkbox:

``` purescript
greetUI :: UI String
greetUI = greet <$> string "Name" "Pierre"
                <*> boolean "French greeting" true
    where greet name french | french    = "Salut " ++ name ++ "!"
                            | otherwise = "Hello " ++ name ++ "!"

main = runFlare "controls" "target" greetUI
```
