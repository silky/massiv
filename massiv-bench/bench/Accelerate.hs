{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Criterion.Main
import           Data.Array.Accelerate             as A
import           Data.Array.Accelerate.LLVM.Native as A
import           Data.Array.Repa                   as R
import           Data.Functor.Identity
import           Prelude                           as P

accArrLightDIM2 :: (Int, Int) -> A.Array A.DIM2 Double
accArrLightDIM2 (m, n) = A.fromFunction (A.Z A.:. m A.:. n) (\ (A.Z A.:. i A.:. j) -> lightFunc i j)

lightFuncExp :: Exp Int -> Exp Int -> Exp Double
lightFuncExp i j =
  A.sin (A.fromIntegral (i A.^ (2 :: Exp Int) + j A.^ (2 :: Exp Int)) :: Exp Double)

sumAccArr :: (A.Elt e, A.Shape t, P.Num (Exp e)) => A.Array t e -> e
sumAccArr = (`indexArray` A.Z) . run . A.sum . A.flatten . use

lightFunc :: Int -> Int -> Double
lightFunc i j = sin (P.fromIntegral (i P.^ (2 :: Int) + j P.^ (2 :: Int)) :: Double)

tupleToSh2 :: (Int, Int) -> R.DIM2
tupleToSh2 (i, j) = R.Z R.:. i R.:. j

arrDLightSh2 :: R.DIM2 -> R.Array D R.DIM2 Double
arrDLightSh2 sz = R.fromFunction sz (\(R.Z R.:. i R.:. j) -> lightFunc i j)

main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
      repaArr = computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))
      accArr = accArrLightDIM2 t2
  defaultMain
    [ bgroup
        "Sum"
        [ bench "Repa DIM2 U" $ whnf (runIdentity . R.sumAllP) repaArr
        , bench "Accelerate DIM2" $ whnf sumAccArr accArr
        , bench "Repa DIM2 U" $ whnf (runIdentity . R.sumAllP) repaArr
        ]
    ]
