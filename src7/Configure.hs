{-# LANGUAGE Arrows #-}
module Configure where

import Linear.V2

-- one unit is equal to 100 meters
wall :: Double
wall  = 0.8

ground :: Double
ground = -0.5

speed :: Double
speed  = 0.4

data Racket = Racket (V2 Double) (V2 Double)  
              deriving (Eq, Show, Read)

corner :: V2 Double
corner = V2 0 (-0.5)

boundary  :: V2 Double
boundary = V2 0.1 0.01

data Ball = Ball (V2 Double) Double  deriving (Eq, Show, Read)

pInit :: V2 Double
pInit = V2 0 0.5

gInit :: V2 Double
gInit = V2 0 (-0.098)

vInit :: V2 Double
vInit = V2 0.1 0

radius :: Double
radius = 0.02
