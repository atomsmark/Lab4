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
     zoomInput <- mkInput 5 "1.0"
     zoomSlider <- mkSlider (1,20) 10
     differentiateButton <- mkButton "Differentiate"

     --scaleInput <- mkInput 5 "Scale"
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx, pure input]
     buttons <- row [pure draw, pure differentiateButton, pure zoomInput, pure zoomSlider] -- Testar med zoomslid + zoominput
     getBody window #+ [column [pure canvas, pure formula, pure buttons]]
     -- test
     row [pure zoomInput, string "Zoom"]


     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input differentiateButton canvas zoomInput zoomSlider 
     on UI.click differentiateButton $ \ _ -> differentiateAndDraw input canvas zoomInput zoomSlider
     on valueChange' input $ \ _ -> readAndDraw input differentiateButton canvas zoomInput zoomSlider
     on valueChange' zoomSlider $ \ _ -> readAndDraw input differentiateButton canvas zoomInput zoomSlider



readAndDraw :: Element -> Element -> Canvas -> Element -> Element -> UI ()
readAndDraw input differentiateButton canvas zoomInput zoomSlider =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.------------------------------------------------------

     case readExpr formula of
      Just expr -> do
        set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
        UI.fillText formula (10,canHeight/2) canvas

        zoomFactorStr <- get value zoomInput
        let zoomFactor = read zoomFactorStr :: Double


        sliderValue <- get value zoomSlider
        let sliderZoomFactor = read sliderValue :: Double
        let totalZoomFactor = zoomFactor * sliderZoomFactor


        let scale = 0.04 * totalZoomFactor
        let pointsList = points expr scale (canWidth, canHeight)
        path "green" pointsList canvas

      Nothing -> do
        set UI.fillStyle (UI.solidColor (UI.RGB 255 0 0)) (pure canvas)
        UI.fillText "Invalid expression" (10,canHeight/2) canvas

differentiateAndDraw :: Element -> Canvas -> Element -> Element -> UI ()
differentiateAndDraw input canvas zoomInput zoomSlider =
  do
    formula <- get value input
    clearCanvas canvas

    case readExpr formula of
      Just expr -> do
        set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
        UI.fillText formula (10, canHeight / 2) canvas

        zoomFactorStr <- get value zoomInput
        let zoomFactor = read zoomFactorStr :: Double


        sliderValue <- get value zoomSlider
        let sliderZoomFactor = read sliderValue :: Double
        let totalZoomFactor = zoomFactor * sliderZoomFactor

        let scale = 0.04 * totalZoomFactor
        let diffExpr = simplify (differentiate expr)
        let pointsList = points diffExpr scale (canWidth, canHeight)

        set value (showExpr diffExpr) (pure input)

        path "red" pointsList canvas

        
        
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