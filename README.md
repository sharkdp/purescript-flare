## Flare

Flare is a special-purpose UI library for
[PureScript](https://github.com/purescript/purescript). It is built on top
of [purescript-signal](https://github.com/bodil/purescript-signal) and uses
Applicative-style programming to combine predefined input fields to a reactive
user interface. Flare is inspired by the Haskell library
[typed-spreadsheet](https://github.com/Gabriel439/Haskell-Typed-Spreadsheet-Library).
The main design-criterion of this library is ease of use.

- [Tutorial](http://david-peter.de/articles/flare/) - Introduction with many examples
- [Tests](http://sharkdp.github.io/purescript-flare/) - A lot of additional examples
- [Try Flare](http://sharkdp.github.io/try-flare/): Write Flare UIs in an online editor
- [Quick start](https://github.com/sharkdp/flare-example) - Start a new Flare project
- [Module documentation](http://pursuit.purescript.org/packages/purescript-flare/)

## Projects that use Flare

- [purescript-flarecheck](https://github.com/sharkdp/purescript-flarecheck) - QuickCheck-style interactive tests
- [purescript-flaredoc](https://github.com/sharkdp/purescript-flaredoc/) - Interactive documentation using FlareCheck (see [-arrays](http://sharkdp.github.io/purescript-flaredoc/), [-strings](http://sharkdp.github.io/purescript-strings/), [-colors](http://sharkdp.github.io/purescript-colors/))
- [purescript-isometric](http://sharkdp.github.io/purescript-isometric/) - Interactive 3D rendering
- [Nature invented it first](http://nosubstance.me/post/nature-invented-it-first/) - Blog post with interactive animation

## Building
```
bower install
pulp build -O -I test -m Test.Main -t html/main.js
```
