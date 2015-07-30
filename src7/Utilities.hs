{-# LANGUAGE Arrows #-}
module Utilities (integral', integralWith', integralWith'') where

import Prelude hiding ((.),)
import Control.Wire
import FRP.Netwire
import Linear.V2

import Configure

integral' :: (HasTime t s) => V2 Double -> Wire s e m (V2 Double) (V2 Double)
integral' x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
        in x' `seq` (Right x', integral' (x' + dt*dx))

integralWith' :: (HasTime t s) => (V2 Bool -> V2 Double -> V2 Double) -> V2 Double -> Wire s e m (V2 Double, V2 Bool) (V2 Double)
integralWith' correct = loop
    where
    loop x' =
        mkPure $ \ds (dx, w) ->
            let dt = realToFrac (dtime ds)
                x  = correct w (x' + dt*dx)
            in x' `seq` (Right x', loop x)

integralWith'' :: (HasTime t s) => (Racket -> V2 Double -> (V2 Double, V2 Bool)) -> V2 Double -> Wire s e m (V2 Double, Racket) (V2 Double, V2 Bool)
integralWith'' correct = loop
  where
    loop x' =
        mkPure $ \ds (dx, p1) ->
            let dt = realToFrac (dtime ds)
                (x,b)  = correct p1 (x' + dt*dx)
            in x' `seq` (Right (x', b), loop x)
