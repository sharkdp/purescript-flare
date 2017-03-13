module Flare.Drawing
  ( runFlareDrawing
  , module Graphics.Drawing
  ) where

import Prelude (Unit, bind)

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Maybe (fromJust)
import Signal.Channel (CHANNEL)
import Partial.Unsafe (unsafePartial)

import Graphics.Drawing
import Graphics.Canvas (getCanvasElementById, getContext2D, CANVAS,
                        getCanvasWidth, getCanvasHeight, clearRect)

import Flare (UI, ElementId, runFlareWith)

-- | Renders a Flare UI with a `Drawing` as output. The first ID specifies
-- | the DOM element for the controls while the second ID specifies the
-- | canvas for rendering.
runFlareDrawing :: forall e. ElementId
                -> ElementId
                -> UI (canvas :: CANVAS | e) Drawing
                -> Eff (dom :: DOM, channel :: CHANNEL, canvas :: CANVAS | e) Unit
runFlareDrawing controls canvasID ui = do
  mcanvas <- getCanvasElementById canvasID
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas

  let render' drawing = do
        w <- getCanvasWidth canvas
        h <- getCanvasHeight canvas
        clearRect ctx { x: 0.0, y: 0.0, w, h }
        render ctx drawing

  runFlareWith controls render' ui
