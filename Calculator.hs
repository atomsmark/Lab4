-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import Graphics.UI.Threepenny

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas


readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.------------------------------------------------------
     
     {-set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" [(10,10),(canWidth-10,canHeight/2)] canvas-}

     let expr = fromJust (readExpr formula)

     let scale = 0.04
     let pointsList = points expr scale (canWidth, canHeight)

     path "blue" pointsList canvas



points :: Expr -> Double -> (Int, Int) -> [Point]
{-The idea is that points will calculate all the points of
  the graph in terms of pixels.
  
  The scaling value tells you the ratio between pixels and
  floating point numbers.
  
  The arguments width and height tell you how big the drawing area is.
  
  We assume that the origin (0,0) point is in the middle of
  our drawing area.
  
  For the canvas and function coordinate systems above,
  we would use 0.04 for the scale (since 1 pixel corresponds to 0.04
  in the floating point world, this is (6.0 + 6.0) / 300), and (300,300)
  for the width and height.-}

  {-KONVERTERING KOORDINATER --> PIXLAR-}
points expr scale (width, height) = [(pixToCoord x, coordToPix (eval expr (pixToCoord x))) | x <- [0..width']]
  where
    width' = fromIntegral width

pixToCoord :: Double -> Double
pixToCoord x = (x - width' / 2) * scale
  where
    width' = fromIntegral width
  
coordToPix :: Double -> Double
coordToPix y = height' / 2 - y / scale
  where
    height' = fromIntegral height