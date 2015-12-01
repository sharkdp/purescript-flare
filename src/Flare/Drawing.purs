module Flare.Drawing
  ( runFlareDrawing
  , module Graphics.Drawing
  ) where

import Prelude

import Control.Monad.Eff

import Data.Maybe

import Graphics.Drawing
import Graphics.Canvas (getCanvasElementById, getContext2D, Canvas(),
                        getCanvasWidth, getCanvasHeight, clearRect)

import DOM

import Signal.Channel

import Flare

-- | Renders a Flare UI with a `Drawing` as output. The first ID specifies
-- | the DOM element for the controls while the second ID specifies the
-- | canvas for rendering.
runFlareDrawing :: forall e. ElementId
                -> ElementId
                -> UI (canvas :: Canvas | e) Drawing
                -> Eff (dom :: DOM, chan :: Chan, canvas :: Canvas | e) Unit
runFlareDrawing controls canvasID ui = do
  Just canvas <- getCanvasElementById canvasID
  ctx <- getContext2D canvas

  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas

  let render' drawing = do
        clearRect ctx { x: 0.0, y: 0.0, w, h }
        render ctx drawing

  runFlareWith controls render' ui
