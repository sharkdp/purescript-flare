module Flare.Drawing
  ( runFlareDrawing
  , module Graphics.Drawing
  ) where

import Graphics.Drawing

import Data.Maybe (fromJust)
import Effect (Effect)
import Flare (UI, ElementId, runFlareWith)
import Graphics.Canvas (getCanvasElementById, getContext2D, getCanvasWidth, getCanvasHeight, clearRect)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind)

-- | Renders a Flare UI with a `Drawing` as output. The first ID specifies
-- | the DOM element for the controls while the second ID specifies the
-- | canvas for rendering.
runFlareDrawing :: ElementId
                -> ElementId
                -> UI Drawing
                -> Effect Unit
runFlareDrawing controls canvasID ui = do
  mcanvas <- getCanvasElementById canvasID
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas

  let render' drawing = do
        w <- getCanvasWidth canvas
        h <- getCanvasHeight canvas
        _ <- clearRect ctx { x: 0.0, y: 0.0, width: w, height: h }
        render ctx drawing

  runFlareWith controls render' ui
