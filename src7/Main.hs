{-# LANGUAGE Arrows #-}
-- module Main where

import Prelude hiding ((.),)
import Control.Wire
import Control.Monad.IO.Class()
import FRP.Netwire()
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLFW 
import Data.IORef
import Linear.V2

import Ball
import Racket
import Configure

type Point = (Double, Double)
type Polygon = [Point]

renderPoint :: Point -> IO () 
renderPoint (x, y) = vertex $ Vertex2 (realToFrac x :: GLfloat) (realToFrac y :: GLfloat)

generatePointsForBall :: Ball -> Polygon 
generatePointsForBall (Ball (V2 x y) r) = 
  map (\t -> (x+r*cos (t), y+r*sin (t))) [0,0.2..(2*pi)]

generatePointsForRacket :: Racket -> Polygon 
generatePointsForRacket (Racket (V2 x y) (V2 w h)) = 
  [ (x, y) 
  , (x + w, y) 
  , (x + w, y + h) 
  , (x, y + h) ] 

generatePointsForLeftWall :: Polygon 
generatePointsForLeftWall = 
  [ (-1, -1) 
  , (- wall, -1) 
  , (- wall, 1) 
  , (-1, 1) ] 

generatePointsForRightWall :: Polygon 
generatePointsForRightWall = 
  [ (1, 1) 
  , (wall, 1) 
  , (wall, -1) 
  , (1, -1) ] 

runNetwork :: (HasTime t s) => IORef Bool -> Session IO s -> Wire s e IO a (Ball, Racket) -> IO () 
runNetwork closedRef session wire = do 
  pollEvents 
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  closed <- readIORef closedRef 
  if closed 
    then return () 
    else do
      (st , session') <- stepSession session 
      (wt', wire' ) <- stepWire wire st $ Right undefined 
      case wt' of 
        Left _ -> return () 
        Right (b,r) -> do
          clear [ColorBuffer] 
          --color (Color3 1 0 0)
          color3f 1.0 0.8 0.6
          renderPrimitive Polygon $ 
            mapM_ renderPoint $ generatePointsForBall b
          color3f 0.8 0.2 0.2
          renderPrimitive Polygon $ 
            mapM_ renderPoint $ generatePointsForRacket r
          color3f 0.7 0.7 0.7
          renderPrimitive Polygon $ 
            mapM_ renderPoint $ generatePointsForLeftWall
          renderPrimitive Polygon $ 
            mapM_ renderPoint $ generatePointsForRightWall
          swapBuffers 
          runNetwork closedRef session' wire' 

game :: HasTime t s => Wire s () IO a (Ball, Racket)
game = proc _ -> do
    r <- racket corner boundary   -< ()
    b <- ball radius gInit vInit pInit -< r
    returnA -< (b, r)

main :: IO () 
main = do
  initialize 
  openWindow (Size 640 640) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  closedRef <- newIORef False 
  windowCloseCallback $= do 
    writeIORef closedRef True 
    return True 
  runNetwork closedRef clockSession_ game
  closeWindow
