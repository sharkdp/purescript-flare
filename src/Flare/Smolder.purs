module Flare.Smolder
  ( runFlareHTML
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Signal.Channel (CHANNEL)

import Text.Smolder.Markup (Markup())
import Text.Smolder.Renderer.String (render)

import Flare (UI, ElementId, runFlare)

-- | Renders a Flare UI with `Markup` as output. The first ID specifies
-- | the DOM element for the controls while the second ID specifies the
-- | element for the output.
runFlareHTML :: forall e. ElementId
                -> ElementId
                -> UI e Markup
                -> Eff (dom :: DOM, channel :: CHANNEL | e) Unit
runFlareHTML controls target =
  runFlare controls target <<< map render
