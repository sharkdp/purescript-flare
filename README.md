## Flare

Flare is a special-purpose UI library for
[PureScript](https://github.com/purescript/purescript). It is built on top
of [purescript-signal](https://github.com/bodil/purescript-signal) and uses
Applicative-style programming to combine predefined input fields to a reactive
user interface. Flare is inspired by the Haskell library
[typed-spreadsheet](https://github.com/Gabriel439/Haskell-Typed-Spreadsheet-Library).
The main design-criterion of this library is ease of use.

- [Tutorial](https://david-peter.de/articles/flare/) - Introduction with many examples
- [Try Flare](http://try.purescript.org/?backend=flare) - Write and compile Flare UIs in your browser
- [Talk](https://www.youtube.com/watch?v=iTSosG7vUyI) - A talk I gave about Flare and FlareCheck at LambdaConf 2016
- [Tests](http://sharkdp.github.io/purescript-flare/) - A lot of additional examples
- [Quick start](https://github.com/sharkdp/flare-example) - Start a new Flare project
- [Module documentation](http://pursuit.purescript.org/packages/purescript-flare/)

## Projects that use Flare

- [purescript-sparkle](https://github.com/sharkdp/purescript-sparkle) - QuickCheck-style interactive tests
- [purescript-flaredoc](https://github.com/sharkdp/purescript-flaredoc/) - Interactive documentation using FlareCheck (see [-arrays](http://sharkdp.github.io/purescript-flaredoc/), [-strings](http://sharkdp.github.io/purescript-strings/), [-colors](http://sharkdp.github.io/purescript-colors/))
- [purescript-isometric](http://sharkdp.github.io/purescript-isometric/) - Interactive 3D rendering
- [Nature invented it first](http://nosubstance.me/post/nature-invented-it-first/) - Blog post with interactive animation

## Building
```
bower install
pulp build -O -I test -m Test.Main -t html/main.js
```
