{-# LANGUAGE Arrows #-}
module Ball (ball) where

import Control.Wire
import Linear.V2

import Utilities
import Configure
import Racket()

gravity :: (HasTime t s, Monad m) => (V2 Double) -> Wire s () m a (V2 Double) 
gravity g = pure g

velocity ::  (HasTime t s, Monad m) => (V2 Double) -> Wire s () m (V2 Double, V2 Bool) (V2 Double)
velocity v = integralWith' bounce v
             where bounce (V2 bx by) v'@(V2 x y) 
                     | bx        = V2 (-x) y
                     | by        = V2 x    (-y)
                     | otherwise = v'

position :: (HasTime t s, Monad m) => (V2 Double) -> Wire s () m (V2 Double, Racket) (V2 Double, V2 Bool)  
position p = integralWith'' clamp p    
 where    
  clamp (Racket (V2 x1 y1) (V2 x2 y2)) p'@(V2 x y) 
    | x < - wall && y > y1 = 
        ((V2 (- wall * 2 - x) y), 
          V2 True  False)
    | x >   wall && y > y1 = 
        ((V2 (  wall * 2 - x) y), 
          V2 True  False)  
    | y > y1 && y < (y1 + y2)  && x > x1 && x < x1 + x2 =
        ((V2 x ( (y1 + y2)   * 2 - y)), 
          V2 False True ) 
    | otherwise = (p', V2 False False)

dynamics :: (HasTime t s) => (V2 Double) -> (V2 Double) -> (V2 Double) -> Wire s () IO Racket (V2 Double) 
dynamics g0 v0 p0 = proc racket -> do
  rec g <- gravity g0  -< undefined
      v <- velocity v0 -< (g, b)
      (p, b) <- position p0 -< (v, racket)
  returnA -< p

ball :: (HasTime t s) => Double -> (V2 Double) -> (V2 Double) -> (V2 Double) -> Wire s () IO Racket Ball
ball r g0 v0 p0 = proc racket -> do
    pos <- dynamics g0 v0 p0 -< racket
    returnA -< makeBall pos
  where makeBall :: V2 Double -> Ball
        makeBall p = Ball p r
