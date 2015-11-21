## Module Flare.Drawing

#### `runFlareDrawing`

``` purescript
runFlareDrawing :: forall e. ElementId -> ElementId -> UI (canvas :: Canvas | e) Drawing -> Eff (dom :: DOM, chan :: Chan, canvas :: Canvas | e) Unit
```

Renders a Flare UI with a `Drawing` as output. The first ID specifies
the DOM element for the controls while the second ID specifies the
canvas for rendering.


