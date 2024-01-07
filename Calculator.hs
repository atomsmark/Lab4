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
     differentiateButton <- mkButton "Differentiate"
     scaleInput <- mkInput 5 "Scale"
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx, pure input]
     buttons <- row [pure draw, pure differentiateButton, pure scaleInput]
     getBody window #+ [column [pure canvas, pure formula, pure buttons]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input differentiateButton scaleInput canvas
     on UI.click differentiateButton $ \ _ -> differentiateAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input differentiateButton scaleInput canvas


readAndDraw :: Element -> Element -> Element -> Canvas -> UI ()
readAndDraw input differentiateButton scaleInput canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     scaleStr <- get value scaleInput
     let scale = read scaleStr :: Double
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.------------------------------------------------------

     case readExpr formula of
      Just expr -> do
        set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
        UI.fillText formula (10,canHeight/2) canvas

        --let scale = 0.04
        let pointsList = points expr scale (canWidth, canHeight)

        path "green" pointsList canvas
      Nothing -> do
        set UI.fillStyle (UI.solidColor (UI.RGB 255 0 0)) (pure canvas)
        UI.fillText "Invalid expression" (10,canHeight/2) canvas

differentiateAndDraw :: Element -> Canvas -> UI ()
differentiateAndDraw input canvas =
  do
    formula <- get value input
    clearCanvas canvas

    case readExpr formula of
      Just expr -> do
        set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
        UI.fillText formula (10, canHeight / 2) canvas

        let scale = 0.04
        let diffExpr = simplify (differentiate expr)
        let pointsList = points diffExpr scale (canWidth, canHeight)

        path "red" pointsList canvas

        --set value (showExpr diffExpr) input
        
      Nothing -> do
        set UI.fillStyle (UI.solidColor (UI.RGB 255 0 0)) (pure canvas)
        UI.fillText "Invalid expression" (10, canHeight / 2) canvas




points :: Expr -> Double -> (Int, Int) -> [Point]
points expr scale (width, height) = [(x, coordToPix (eval expr (pixToCoord x))) | x <- [0..fromIntegral width]]
  where
    pixToCoord :: Double -> Double
    pixToCoord x = (x - fromIntegral width / 2) * scale
  
    coordToPix :: Double -> Double
    coordToPix y = fromIntegral height / 2 - y / scale