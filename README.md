## Flare

Flare is a special-purpose UI library for
[Purescript](https://github.com/purescript/purescript). It is built on top
of [purescript-signal](https://github.com/bodil/purescript-signal) and uses
Applicative-style programming to combine predefined input fields to a reactive
user interface. Flare is inspired by the Haskell library
[typed-spreadsheet](https://github.com/Gabriel439/Haskell-Typed-Spreadsheet-Library).
The main design-criterion of this library is ease of use.

- [Introduction and tutorial with many examples](http://david-peter.de/articles/flare/)
- [Tests with additional examples](http://sharkdp.github.io/purescript-flare/)
- [Quick start project](https://github.com/sharkdp/flare-example)
- [Module documentation](http://pursuit.purescript.org/packages/purescript-flare/)


## Example

![](http://i.imgur.com/YTQjTG8.png)

A basic Flare UI with two input fields can be created with just a few lines:

``` purescript
module Main where

import Prelude
import Flare
import Math (pow)

main = runFlare "controls" "output" $
         pow <$> number "Base" 2.0
             <*> number "Exponent" 10.0
```

Here, `controls` and `output` are IDs of two `<div>` elements in the
corresponding HTML file. The input fields will be appended to `controls` while
the current output will be rendered to the `output` element.

## Building
```
bower install
pulp build -O -I test -m Test.Main -t html/main.js
```
