{-# LANGUAGE ScopedTypeVariables #-}

module FitCuboidBFGS
  ( Cuboid
  , errfun
  , cuboidFromParams
  , fitCuboid
  , fitCuboidFromCenter
  , fitCuboidFromCenterFirst
  , fitCuboidFromCenterFirstError
  , main
  ) where

import Control.Applicative
import Control.Monad
import Data.List (sort, foldl', minimumBy)
import Data.Ord (comparing)
import Data.Packed.Matrix
import Numeric.GSL.Minimization
import Data.Vect.Double hiding (Matrix)
import Data.Vect.Double.Util.Quaternion
-- import Graphics.Plot (mplot)
import Test.QuickCheck
-- import Text.Printf


type Cuboid = [Vec3]

_example_points :: Cuboid
_example_points = map (.* rotMat) $
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


errfunClosest :: Cuboid -> [Double] -> Double
errfunClosest ps (x:y:z:params) = errfunClosestCenter (Vec3 x y z) ps params
errfunClosest _ _ = error "errfunClosest: bad params"

-- Just takes the closest point as correspondent
errfunClosestCenter :: Vec3 -> Cuboid -> [Double] -> Double
errfunClosestCenter (Vec3 x y z) ps params = sum [ normsqr ( p &- e ) | p <- ps, let e = minimumBy (comparing (distance p)) est ]
  where
    est = cuboidFromParams ([x,y,z] ++ params)



pointMean :: [Vec3] -> Vec3
pointMean points = c
  where
    n = length points
    c = foldl' (&+) zero points &* (1 / fromIntegral n)  -- bound center

-- | Rotates a point around a rotation center.
rotateAround :: Vec3 -> Mat3 -> Vec3 -> Vec3
rotateAround rotCenter rotMat p = ((p &- rotCenter) .* rotMat) &+ rotCenter


cuboidFromParams :: [Double] -> Cuboid
cuboidFromParams [x,y,z, a,b,c, q1,q2,q3,q4] = ps
  where
    rotMat = fromOrtho (rightOrthoU (mkU (Vec4 q1 q2 q3 q4)))
    ps = map (rotateAround (Vec3 x y z) rotMat) $
           [ Vec3 (x - a/2) (y - b/2) (z - c/2)
           , Vec3 (x - a/2) (y - b/2) (z + c/2)
           , Vec3 (x - a/2) (y + b/2) (z - c/2)
           , Vec3 (x - a/2) (y + b/2) (z + c/2)
           , Vec3 (x + a/2) (y - b/2) (z - c/2)
           , Vec3 (x + a/2) (y - b/2) (z + c/2)
           , Vec3 (x + a/2) (y + b/2) (z - c/2)
           , Vec3 (x + a/2) (y + b/2) (z + c/2)
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


-- Fixes the center to the mean of the points and uses closest points as associations.
fitCuboidFromCenter :: Cuboid -> ([Double], Int, Double, Matrix Double)
fitCuboidFromCenter points = ([x,y,z] ++ solution, rows path, errf solution, path)
  where
    maxIt = 2000
    center@(Vec3 x y z) = pointMean points
    (a,_,_) = guessDims points

    initial = [a,a,a, 0.1,0.1,0.1,0.1]

    errf = errfunClosestCenter center points

    initialSearchBox = [a/10,a/10,a/10, 0.1,0.1,0.1,0.1] -- step_size in GSL
    (solution, path) = minimize NMSimplex2 1e-8 maxIt initial errf initialSearchBox


-- Like fitCuboidFromCenter (closest points as associations), but afterwards allows the center to move.
fitCuboidFromCenterFirst :: Cuboid -> ([Double], Int, Double, Matrix Double)
fitCuboidFromCenterFirst points = (solution, steps1 + rows path, errf solution, path)
  where
    maxIt = 2000
    (a,_,_) = guessDims points

    -- TODO check if fitCuboidFromCenter can overfit, such that
    --      the center is not allowed to move any more afterwards
    (initial, steps1, _, _) = fitCuboidFromCenter points

    errf = errfunClosest points

    initialSearchBox = [0.01,0.01,0.01, a/10,a/10,a/10, 0.1,0.1,0.1,0.1] -- step_size in GSL
    (solution, path) = minimize NMSimplex2 1e-8 maxIt initial errf initialSearchBox



fitCuboid :: Cuboid -> ([Double], Int, Double, Matrix Double)
fitCuboid points = (solution, rows path, errfun points solution, path)
-- fitCuboid unordered = (solution, rows path, errfun points solution, path)
  where
    -- TODO More robust idea: Try all 8! assignments after having used guessDims
    -- f:rest = unordered
    -- [p2,p3, q1,q2,q3,q4, p8] = sortBy (comparing (distance f)) rest
    -- p4 = minimumBy (comparing (distance ( p2 &+ (p3 &- f) ))) [q1,q2,q3,q4]
    -- rem1 = [q1,q2,q3,q4] \\ [p4]
    -- [p5, r1,r2] = minimumBy (comparing (distance f)) rem1

    -- points = [p1, p2, p3, p4, ]


    maxIt = 2000
    -- initial = [1,1,1, 1,1,1, 0,0,0, 1]
    -- initial = replicate 10 0.1
    -- initial = [0.1,0.1,0.1, 0.1,0.1,0.1, 0.1,0.1,0.1, 10]
    (a,b,c) = guessDims points
    -- initial = [0.1,0.1,0.1, a,b,c, 0.1,0.1,0.1, 10]

    -- Vec3 x y z = points!!0 -- TODO changing this makes a difference in how well it converges!?
    Vec3 x y z = pointMean points
    initial = [x,y,z, a,b,c, 0.1,0.1,0.1, 0.1] -- works quite well
    -- initial = [x,y,z, a,b,c, 1, 0, 0, 0] -- 1 0 0 0 is the identity quaternion - this doesn't seem to work too well, 0.1 everywhere works better

    -- initialSearchBox = [0.01,0.01,0.01,0.01,0.01,0.01, 0.1,0.1,0.1,0.1] -- step_size in GSL
    initialSearchBox = [0.01,0.01,0.01, a/10,a/10,a/10, 0.1,0.1,0.1,0.1] -- step_size in GSL
    (solution, path) = minimize NMSimplex2 1e-8 maxIt initial (errfun points) initialSearchBox


fitCuboidFromCenterFirstError :: [Vec3] -> (Double, Int)
fitCuboidFromCenterFirstError ps = let (_params, steps, err, _) = fitCuboidFromCenterFirst ps in (err, steps)


nice :: Double -> Double
nice = (/ 1e2) . dbl . round . (* 1e2)
  where
    dbl :: Int -> Double
    dbl = fromIntegral


guessDims :: Cuboid -> (Double, Double, Double)
guessDims p = (a, b, c)
  where
    f:rest = p
    [a,b,_,_,_,_,d] = sort $ map (distance f) rest
    c = sqrt (d*d - a*a - b*b)


main :: IO ()
main = do
  do -- Example
    let p = _example_points
        (solution, steps, err, _path) = fitCuboid p
        correct = [0,0,0, 2,1,1, 1,2,3,20]
    -- print solution
    putStrLn $ "Solution: " ++ show (map nice solution)
    putStrLn $ "Correct:  " ++ show correct
    putStrLn $ "err (sol): " ++ show err
    putStrLn $ "err (cor): " ++ show (errfun p correct)
    putStrLn $ "steps needed: " ++ show steps
  -- print path
  -- mplot $ drop 3 (toColumns path)

  replicateM_ 10 $ do
    cubs <- sample' cuboidGen
    forM_ cubs $ \(c, (abc,_, _)) -> do
      let (sol, steps, err, _) = fitCuboidFromCenter c
      print (err, steps)
      when (err > 1) $ do
        putStrLn $ "points:\n" ++ show (map (mapVec nice) c)
        -- putStrLn $ "bad sol:\n" ++ show (map nice sol)
        putStrLn $ "wrong points:\n" ++ show (map (mapVec nice) (cuboidFromParams sol))
        putStrLn $ "abc: " ++ show abc
