{-# LANGUAGE Arrows #-}

module Racket (racket) where

import Graphics.Rendering.OpenGL 
import Graphics.UI.GLFW 
import Prelude hiding ((.))
import Control.Wire 
import FRP.Netwire 
import Data.IORef 
import Linear.V2

import Configure

isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a e 
isKeyDown k = mkGen_ $ \_ -> do 
  s <- getKey k 
  return $ case s of 
    Press   -> Right mempty
    Release -> Left mempty 

velocity' :: Wire s () IO a (V2 Double) 
velocity' = 
     pure (V2   0  0) .
      isKeyDown (CharKey 'A') . 
      isKeyDown (CharKey 'D') 
 <|> pure (V2 (- speed) 0) . 
      isKeyDown (CharKey 'A') 
 <|> pure (V2   speed  0) . 
      isKeyDown (CharKey 'D') 
 <|> pure (V2   0  0) 

event :: (HasTime t s) => (V2 Double) -> Wire s () IO a (V2 Double) 
event p0 = integral p0 . velocity'

racket :: (HasTime t s) => (V2 Double) -> (V2 Double) -> Wire s () IO a Racket
racket p0 wh = proc _ -> do
    p <- event p0 -< undefined
    returnA -< makeRacket p wh
  where makeRacket :: V2 Double -> 
         V2 Double -> Racket
        makeRacket p' wh'= Racket p' wh'
