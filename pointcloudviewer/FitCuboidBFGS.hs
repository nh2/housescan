{-# LANGUAGE ScopedTypeVariables #-}

module FitCuboidBFGS
  ( Cuboid
  , errfun
  , cuboidFromParams
  , fitCuboid
  ) where

import Control.Applicative
import Control.Monad
import Data.List (sort)
import Data.Packed.Matrix
import Numeric.GSL.Minimization
import Data.Vect.Double hiding (Matrix)
import Data.Vect.Double.Util.Quaternion
-- import Graphics.Plot (mplot)
import Test.QuickCheck
-- import Text.Printf


type Cuboid = [Vec3]

example_points :: Cuboid
example_points = map (.* rotMat) $
  [ Vec3 0 0 0
  , Vec3 0 0 1
  , Vec3 0 1 0
  , Vec3 0 1 1
  , Vec3 2 0 0
  , Vec3 2 0 1
  , Vec3 2 1 0
  , Vec3 2 1 1
  ]
  where
    rotMat = rotMatrix3 (Vec3 1 2 3) (toRad 20)

toRad :: Double -> Double
toRad d = d / 180 * pi


-- TODO make it so that the rotation comes first,
--      then translation, then side scaling,
--      with xyz describing the center

errfun :: Cuboid -> [Double] -> Double
-- errfun p [x,y,z,a,b,c, r1,r2,r3,theta]
errfun ps params = sum [ normsqr ( p &- e ) | (p,e) <- zip ps est ]
  -- = normsqr (  (p!0) &- rot (Vec3 (x  ) (y  ) (z  ))  )
  -- + normsqr (  (p!1) &- rot (Vec3 (x  ) (y  ) (z+c))  )
  -- + normsqr (  (p!2) &- rot (Vec3 (x  ) (y+b) (z  ))  )
  -- + normsqr (  (p!3) &- rot (Vec3 (x  ) (y+b) (z+c))  )
  -- + normsqr (  (p!4) &- rot (Vec3 (x+a) (y  ) (z  ))  )
  -- + normsqr (  (p!5) &- rot (Vec3 (x+a) (y  ) (z+c))  )
  -- + normsqr (  (p!6) &- rot (Vec3 (x+a) (y+b) (z  ))  )
  -- + normsqr (  (p!7) &- rot (Vec3 (x+a) (y+b) (z+c))  )
  where
    -- rot = (.* rotMatrix3 (Vec3 r1 r2 r3) (toRad theta))
    -- rot = (.* fromOrtho (rightOrthoU (mkU (Vec4 q1 q2 q3 q4))))
    est = cuboidFromParams params


cuboidFromParams :: [Double] -> Cuboid
cuboidFromParams [x,y,z, a,b,c, q1,q2,q3,q4] = ps
  where
    rotMat = fromOrtho (rightOrthoU (mkU (Vec4 q1 q2 q3 q4)))
    ps = map (.* rotMat) $
           [ Vec3 (x  ) (y  ) (z  )
           , Vec3 (x  ) (y  ) (z+c)
           , Vec3 (x  ) (y+b) (z  )
           , Vec3 (x  ) (y+b) (z+c)
           , Vec3 (x+a) (y  ) (z  )
           , Vec3 (x+a) (y  ) (z+c)
           , Vec3 (x+a) (y+b) (z  )
           , Vec3 (x+a) (y+b) (z+c)
           ]
cuboidFromParams _ = error "bad arguments passed to cuboidFromParams"


cuboidGen :: Gen (Cuboid, ((Double, Double, Double), Vec3, Double))
cuboidGen = do
  -- scale <- choose (0.001, 1000)
  let scale = 1 -- TODO sets some scale but not that big
  a <- (* scale) <$> choose (1, 10)
  b <- (* scale) <$> choose (1, 10)
  c <- (* scale) <$> choose (1, 10)
  -- [x,y,z] <- vectorOf 3 (choose (0.001, 1000))
  let [x,y,z] = [0,0,0]
  let ps =
        [ Vec3 (x  ) (y  ) (z  )
        , Vec3 (x  ) (y  ) (z+c)
        , Vec3 (x  ) (y+b) (z  )
        , Vec3 (x  ) (y+b) (z+c)
        , Vec3 (x+a) (y  ) (z  )
        , Vec3 (x+a) (y  ) (z+c)
        , Vec3 (x+a) (y+b) (z  )
        , Vec3 (x+a) (y+b) (z+c)
        ]
  [r1, r2, r3] <- vectorOf 3 (choose (0, 3)) -- TODO negative
  theta <- choose (0, 360)
  let rotAxis = Vec3 r1 r2 r3
      rotMat = rotMatrix3 rotAxis (toRad theta)
      cuboid = map (.* rotMat) ps
      [a',b',c'] = sort [a,b,c]
  return $ (cuboid, ((a',b',c'), rotAxis, theta))


fitCuboid :: Cuboid -> ([Double], Int, Matrix Double)
fitCuboid points = (solution, rows path, path)
  where
    maxIt = 2000
    -- initial = [1,1,1, 1,1,1, 0,0,0, 1]
    -- initial = replicate 10 0.1
    -- initial = [0.1,0.1,0.1, 0.1,0.1,0.1, 0.1,0.1,0.1, 10]
    (a,b,c) = guessDims points
    -- initial = [0.1,0.1,0.1, a,b,c, 0.1,0.1,0.1, 10]
    Vec3 x y z = points!!0 -- TODO changing this makes a difference in how well it converges!?
    initial = [x,y,z, a,b,c, 0.1,0.1,0.1, 0.1]
    -- initialSearchBox = [0.01,0.01,0.01,0.01,0.01,0.01, 0.1,0.1,0.1,0.1] -- step_size in GSL
    initialSearchBox = [0.01,0.01,0.01, a/10,a/10,a/10, 0.1,0.1,0.1,0.1] -- step_size in GSL
    (solution, path) = minimize NMSimplex2 1e-8 maxIt initial (errfun points) initialSearchBox


fitCuboidError :: [Vec3] -> (Double, Int)
fitCuboidError ps = let (params, steps, _) = fitCuboid ps
                     in (errfun ps params, steps)


nice :: Double -> Double
nice = (/ 1e2) . dbl . round . (* 1e2)
  where
    dbl :: Int -> Double
    dbl = fromIntegral

vecmap :: (Double -> Double) -> Vec3 -> Vec3
vecmap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)


guessDims :: Cuboid -> (Double, Double, Double)
guessDims p = (a, b, c)
  where
    f:rest = p
    [a,b,_,_,_,_,d] = sort $ map (distance f) rest
    c = sqrt (d*d - a*a - b*b)


main :: IO ()
main = do
  do -- Example
    let p = example_points
        (solution, steps, _path) = fitCuboid p
        correct = [0,0,0, 2,1,1, 1,2,3,20]
    -- print solution
    putStrLn $ "Solution: " ++ show (map nice solution)
    putStrLn $ "Correct:  " ++ show correct
    putStrLn $ "err (sol): " ++ show (errfun p solution)
    putStrLn $ "err (cor): " ++ show (errfun p correct)
    putStrLn $ "steps needed: " ++ show steps
  -- print path
  -- mplot $ drop 3 (toColumns path)

  replicateM_ 10 $ do
    cubs <- sample' cuboidGen
    forM_ cubs $ \(c, (abc,_, _)) -> do
      let (sol, steps, _) = fitCuboid c
      print (errfun c sol, steps)
      when (errfun c sol > 1) $ do
        putStrLn $ "points:\n" ++ show (map (vecmap nice) c)
        -- putStrLn $ "bad sol:\n" ++ show (map nice sol)
        putStrLn $ "wrong points:\n" ++ show (map (vecmap nice) (cuboidFromParams sol))
        putStrLn $ "abc: " ++ show abc
