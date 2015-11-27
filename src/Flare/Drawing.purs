module Flare.Drawing
  ( runFlareDrawing
  , module Graphics.Drawing
  ) where

import Prelude

import Control.Applicative.Free
import Control.Monad.Eff

import Data.Array (reverse)
import Data.Maybe
import Data.Foldable (traverse_)

import Graphics.Drawing
import Graphics.Canvas (getCanvasElementById, getContext2D, Canvas(),
                        getCanvasWidth, getCanvasHeight, clearRect)

import DOM

import qualified Signal as S
import Signal.Channel

import Flare

-- | Renders a `Flare` with a `Drawing` as output. The first ID specifies
-- | the DOM element for the controls while the second ID specifies the
-- | canvas for rendering.
runFlareDrawing :: forall e. ElementId
                -> ElementId
                -> Flare Drawing
                -> Eff (dom :: DOM, chan :: Chan, canvas :: Canvas | e) Unit
runFlareDrawing controls canvasID (Flare flare) =
  case foldFreeAp cellToUI flare of
    (SetupUI setup) -> do
      (UI els sig) <- setup
      traverse_ (appendComponent controls) (reverse els)

      Just canvas <- getCanvasElementById canvasID
      ctx <- getContext2D canvas

      w <- getCanvasWidth canvas
      h <- getCanvasHeight canvas

      let render' drawing = do
            clearRect ctx { x: 0.0, y: 0.0, w, h }
            render ctx drawing

      S.runSignal (map render' sig)
