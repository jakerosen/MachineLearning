{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Main where

import Numeric.LinearAlgebra
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.Vector.Storable as Vector (take)
import Control.Monad
import Data.Traversable
import Data.Foldable
import Debug.Trace
-- import Control.Lens
import Ex2
import Ex3

main :: IO ()
main = do
  x' :: Matrix Double <-
    readMatrix ("data/ex3data1X.txt")

  let
    x :: Matrix Double
    x = 1 ||| x'

  y :: Vector Double <-
    readVector ("data/ex3data1Y.txt")

  let
    t :: Vector Double
    t = vector (replicate 401 1)

  let

    oneVsAll :: Double -> [Vector Double]
    oneVsAll k =
      let
        y' :: Vector Double
        y' = cmap (\v -> if v == k then 1 else 0) y

        -- t5 :: [Double]
        -- t5 = take 5 $ toList t'

        t' :: [Vector Double]
        t' = logisticRegularizedDescent α l x y' t
      in
        -- traceShow t5 t'
        t'

    result :: [[Vector Double]]
    result = map oneVsAll [1..10]

    test1 :: [Vector Double]
    test1 = oneVsAll 1

  for_ (take 25 test1) $ \vec ->
    print (Vector.take 5 vec)
  -- pure test1
  -- pure ()
  -- print $ result
  -- print $ take 10 $ toList $ result !! 0

  -- Ex2 ----------------------------------------------------------------------
  -- (t, x, y) <- readTrainingData "data/ex2data1.txt"
  -- -- print (logisticCost t x y)

  -- let
  --   loop :: [Vector Double] -> IO ()
  --   loop [] = pure ()
  --   loop (t:ts) = do
  --     -- print t
  --     print (logisticRegularizedCost t x y l)
  --     -- _ <- getLine
  --     loop ts

  -- -- print (logisticRegularizedCost t x y 0.01)
  -- -- print (logisticRegularizedUpdate α 0.01 x y t)
  -- loop (logisticRegularizedDescent α l x y t)
  -- Ex2 ----------------------------------------------------------------------

  -- let
  --   t = vector [1, 0.75, 0.5]
  --   x = (2><3) [1..]
  -- print $ h t x
  --
  -- contents :: Text <- T.readFile "./data1"
  -- let
  --   list :: ([[Double]], [[Double]])
  --   list =
  --     unzip $
  --     map (splitAt 2) $
  --     (map . map) (read . T.unpack) $
  --     T.splitOn "," <$> T.lines contents

  --   x' :: Matrix Double
  --   x' = fromColumns $ map Main.normalize (toColumns (fromLists (fst list)))

  --   x :: Matrix Double
  --   x = (col (replicate (size y) 1)) ||| x'

  --   y :: Vector Double
  --   y = vector (concat (snd list))

  --   t :: Vector Double
  --   t = vector [1, 1, 1]

  --   testX :: Matrix Double
  --   testX = (col (replicate (size testY) 1)) ||| (3><2) [1..]

  --   testY :: Vector Double
  --   testY = vector [1, 2, 3]

  --   -- t0 :: [Vector Double]
  --   -- t0 = gradDescent α testX testY t

  --   t0 :: [Vector Double]
  --   t0 = gradDescent α x y t

  --   loop :: [Vector Double] -> IO ()
  --   loop [] = pure ()
  --   loop (t:ts) = forever $ do
  --     print t
  --     print (cost x y t)
  --     _ <- getLine
  --     loop ts

  -- -- print x
  -- print $ t0 !! 10000
  -- loop t0

readMatrix
  :: FilePath
  -> IO (Matrix Double)
readMatrix path = do
  bytes :: Text <-
    Text.readFile path

  let
    rows :: [Text]
    rows = Text.lines bytes

    matText :: [[Text]]
    matText = map (Text.splitOn ",") rows

    mat :: [[Double]]
    mat = (map . map) t2d matText

  pure (fromLists mat)

readVector
  :: FilePath
  -> IO (Vector Double)
readVector path = do
  bytes :: Text <-
    Text.readFile path

  let
    ys :: [Text]
    ys = Text.lines bytes


    vec :: [Double]
    vec = map t2d ys

  pure (fromList vec)

-- | Read training data that is of the form X1,X2,...,Xn,Y. Prepends a column of
-- 1s to X.
readTrainingData
 :: FilePath
 -> IO ( Vector Double
       , Matrix Double
       , Vector Double
       )
readTrainingData path = do
 bytes :: Text <-
   Text.readFile path

 let as = Text.lines bytes :: [Text]
 let bs = map (Text.splitOn ",") as :: [[Text]]
 let bs' = (map.map) t2d bs
 let cs = map splitLast bs' :: [([Double], Double)]
 let (ds, es) = unzip cs :: ([[Double]], [Double])
 let ds' = Main.normalize (fromLists ds)
 let x = col (replicate (length es) 1) ||| ds'
 let y = vector es
 let t = vector (replicate (1 + length (head ds)) 1)
 pure (t, x, y)

splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast: []"
splitLast [x] = ([], x)
splitLast (x:xs) = let (ys, y) = splitLast xs in (x:ys, y)
-- splitLast (x:xs) = over _1 (x:) (splitLast xs)

-- t2d :: Text -> Double
-- t2d = read . Text.unpack

t2d :: Text -> Double
t2d = either undefined fst . Text.signed Text.double

normalizeV :: Vector Double -> Vector Double
normalizeV v = (v - vector [mean v]) / vector [Main.range v]

normalize :: Matrix Double -> Matrix Double
normalize =
 fromColumns . map normalizeV . toColumns

mean :: Vector Double -> Double
mean v = sumElements v / (fromIntegral (size v))

range :: Vector Double -> Double
range v = (maxElement v) - (minElement v)

α :: Double
α = 0.01

l :: Double
l = 0.01

cost :: Matrix Double -> Vector Double -> Vector Double -> Double
cost x y t = (sumElements $ (x #> t - y)^2) / m
  where
    m :: Double
    m = fromIntegral (size y)

gUpdate
  :: Double          -- alpha
  -> Matrix Double   -- x
  -> Vector Double   -- y
  -> Vector Double   -- t
  -> Vector Double   -- t’
gUpdate a x y t =
  t - scale (a / m) ((x #> t - y) <# x)
    where
      m :: Double
      m = fromIntegral (size y)

gradDescent
  :: Double          -- alpha
  -> Matrix Double   -- x
  -> Vector Double   -- y
  -> Vector Double   -- t
  -> [Vector Double] -- [t’]
gradDescent a x y = iterate (gUpdate a x y)
