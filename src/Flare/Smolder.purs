module Flare.Smolder
  ( runFlareHTML
  ) where

import Prelude

import Effect (Effect)
import Flare (UI, ElementId, runFlare)
import Text.Smolder.Markup (Markup)
import Text.Smolder.Renderer.String (render)

-- | Renders a Flare UI with `Markup` as output. The first ID specifies
-- | the DOM element for the controls while the second ID specifies the
-- | element for the output.
runFlareHTML :: forall e. ElementId
             -> ElementId
             -> UI (Markup e)
             -> Effect Unit
runFlareHTML controls target =
  runFlare controls target <<< map render
