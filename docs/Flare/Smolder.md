## Module Flare.Smolder

#### `runFlareHTML`

``` purescript
runFlareHTML :: forall e. ElementId -> ElementId -> UI e Markup -> Eff (dom :: DOM, channel :: CHANNEL | e) Unit
```

Renders a Flare UI with `Markup` as output. The first ID specifies
the DOM element for the controls while the second ID specifies the
element for the output.


