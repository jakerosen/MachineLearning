module Ex2
  ( logisticCost
  , logisticGradDescent
  , logisticRegularizedCost
  , logisticRegularizedUpdate
  , logisticRegularizedDescent
  ) where

import Numeric.LinearAlgebra
import Data.Vector.Storable ((//))

h :: Vector Double -> Matrix Double -> Vector Double
h t x = g (x #> t)
  where
    g :: Vector Double -> Vector Double
    g z = 1 / (1 + exp (-z))

logisticCost
  :: Vector Double -- t
  -> Matrix Double -- x
  -> Vector Double -- y
  -> Double
logisticCost t x y = 1 / m * cost
  where
    m = fromIntegral (size y)

    cost = (-y) <.> log hy - (1 - y) <.> log (1 - hy)

    hy = h t x

logisticRegularizedCost
  :: Vector Double -- t
  -> Matrix Double -- x
  -> Vector Double -- y
  -> Double        -- l
  -> Double
logisticRegularizedCost t x y l = 1 / m * (cost + reg)
  where
    m = fromIntegral (size y)

    cost = (-y) <.> log hy - (1 - y) <.> log (1 - hy)
    reg = l / (2) * sumElements (subVector 1 (size t - 1) t^2)

    hy = h t x

gUpdate
  :: Double          -- alpha
  -> Matrix Double   -- x
  -> Vector Double   -- y
  -> Vector Double   -- t
  -> Vector Double   -- t’
gUpdate a x y t =
  t - scale (a / m) ((h t x - y) <# x)
    where
      m :: Double
      m = fromIntegral (size y)

logisticGradDescent
  :: Double          -- alpha
  -> Matrix Double   -- x
  -> Vector Double   -- y
  -> Vector Double   -- t
  -> [Vector Double] -- [t’]
logisticGradDescent a x y = iterate (gUpdate a x y)

logisticRegularizedUpdate
  :: Double          -- alpha
  -> Double          -- l
  -> Matrix Double   -- x
  -> Vector Double   -- y
  -> Vector Double   -- t
  -> Vector Double   -- t’
logisticRegularizedUpdate a l x y t =
  t - scale (a / m) (((h t x - y) <# x) + reg)
    where
      m :: Double
      m = fromIntegral (size y)

      reg :: Vector Double
      reg = scale l (t // [(0, 0)])

logisticRegularizedDescent
  :: Double          -- alpha
  -> Double          -- l
  -> Matrix Double   -- x
  -> Vector Double   -- y
  -> Vector Double   -- t
  -> [Vector Double] -- [t’]
logisticRegularizedDescent a l x y = iterate (logisticRegularizedUpdate a l x y)

    -- f = (-y) * log hy - (1 - y) * log (1 - hy)
