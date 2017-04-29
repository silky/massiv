{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv.Ops.Fold
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Fold
  ( -- * Monadic folds
    foldlM
  , foldlM_
  , ifoldlM
  , ifoldlM_
  -- * Sequential folds
  , foldlS
  , foldrS
  , ifoldlS
  , ifoldrS
  , eqS
  , sumS
  , productS
  -- * Parallel folds
  , foldlP
  , foldrP
  , ifoldlP
  , ifoldrP
  , foldP
  , eqP
  , sumP
  , productP
  ) where

import           Control.Monad               (void, when)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Ops.Map   as M (zipWith)
import           Data.Array.Massiv.Scheduler
import qualified Data.Foldable               as F (foldl', foldr')
import           Data.List                   (sortOn)
import           System.IO.Unsafe            (unsafePerformIO)
-- TODO: Use CPP to account for sortOn is only available since base-4.8
--import Data.List (sortBy)
--import Data.Function (on)


-- | /O(n)/ - Monadic left fold.
foldlM :: (Source r ix b, Monad m) => (a -> b -> m a) -> a -> Array r ix b -> m a
foldlM f !acc !arr =
  iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a (unsafeIndex arr ix)
{-# INLINE foldlM #-}


-- | /O(n)/ - Monadic left fold, that discards the result.
foldlM_ :: (Source r ix b, Monad m) => (a -> ix -> b -> m a) -> a -> Array r ix b -> m ()
foldlM_ f !acc !arr =
  void $ iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE foldlM_ #-}


-- | /O(n)/ - Monadic left fold with an index aware function.
ifoldlM :: (Source r ix b, Monad m) => (a -> ix -> b -> m a) -> a -> Array r ix b -> m a
ifoldlM f !acc !arr =
  iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE ifoldlM #-}


-- | /O(n)/ - Monadic left fold with an index aware function, that discards the result.
ifoldlM_ :: (Source r ix b, Monad m) => (a -> ix -> b -> m a) -> a -> Array r ix b -> m ()
ifoldlM_ f !acc !arr =
  void $ iterM zeroIndex (size arr) 1 (<) acc $ \ !ix !a -> f a ix (unsafeIndex arr ix)
{-# INLINE ifoldlM_ #-}


-- | /O(n)/ - Left fold, computed sequentially.
foldlS
  :: Source r ix e
  => (a -> e -> a) -> a -> Array r ix e -> a
foldlS f !acc !arr =
  iter zeroIndex (size arr) 1 (<) acc $ \ !ix !acc0 -> f acc0 (unsafeIndex arr ix)
{-# INLINE foldlS #-}


-- | /O(n)/ - Right fold, computed sequentially.
foldrS
  :: Source r ix e
  => (e -> a -> a) -> a -> Array r ix e -> a
foldrS f !acc !arr =
  iter (liftIndex (subtract 1) (size arr)) zeroIndex (-1) (>=) acc $ \ !ix !acc0 ->
    f (unsafeIndex arr ix) acc0
{-# INLINE foldrS #-}


-- | /O(n)/ - Left fold with an index aware function, computed sequentially.
ifoldlS
  :: Source r ix e
  => (a -> ix -> e -> a) -> a -> Array r ix e -> a
ifoldlS f !acc !arr =
  iter zeroIndex (size arr) 1 (<) acc $ \ !ix !acc0 -> f acc0 ix (unsafeIndex arr ix)
{-# INLINE ifoldlS #-}


-- | /O(n)/ - Right fold with an index aware function, computed sequentially.
ifoldrS
  :: Source r ix e
  => (ix -> e -> a -> a) -> a -> Array r ix e -> a
ifoldrS f !acc !arr =
  iter (liftIndex (subtract 1) (size arr)) zeroIndex (-1) (>=) acc $ \ !ix !acc0 ->
    f ix (unsafeIndex arr ix) acc0
{-# INLINE ifoldrS #-}


-- | /O(n1 + n2)/ - Compute equlity sequentially.
eqS :: (Eq a, Source r1 ix a, Source r2 ix a) =>
       Array r1 ix a -> Array r2 ix a -> Bool
eqS !arr1 !arr2 = (size arr1 == size arr2) && foldlS (&&) True (M.zipWith (==) arr1 arr2)
{-# INLINE eqS #-}


-- | /O(n)/ - Compute sum sequentially.
sumS :: (Source r ix e, Num e) =>
        Array r ix e -> e
sumS = foldlS (+) 0
{-# INLINE sumS #-}


-- | /O(n)/ - Compute product sequentially.
productS :: (Source r ix e, Num e) =>
            Array r ix e -> e
productS = foldlS (*) 1
{-# INLINE productS #-}



-- | /O(n)/ - Left fold, computed in parallel. Parallelization of folding
-- is implemented in such a way that an array is split into a number of chunks
-- of equal length, plus an extra one for the remainder. Number of chunks is the
-- same as number of available cores (capabilities) plus one, and each chunk is
-- individually folded by a separate core with a function @g@. Results from
-- folding each chunk are further folded with another function @f@, thus
-- allowing us to use information about the strucutre of an array during
-- folding.
--
-- ==== __Examples__
--
-- Emulate different number of cores. (/Note/: @setNumCapabilities@ does not
-- actually affect number of parallel workers)
--
-- >>> :m Control.Concurrent
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9,8,7,6,5,4,3,2,1,0]]
-- >>> setNumCapabilities 3
-- >>> foldlP (flip (:)) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9],[8,7,6],[5,4,3],[2,1,0]]
--
-- The order in which chunks folding results will be supplied to function @f@ is
-- guaranteed to be consecutive, i.e. aligned with folding direction.
--
-- >>> setNumCapabilities 4
-- >>> foldlP (flip (:) . reverse) [] (flip (:)) [] $ makeArray1D 11 id
-- [[10,9,8],[5,4],[1,0],[3,2],[7,6]]
--
foldlP :: Source r ix e =>
          (b -> a -> b) -- ^ Chunk results folding function @f@.
       -> b -- ^ Accumulator for results of chunks folding.
       -> (a -> e -> a) -- ^ Chunks folding function @g@.
       -> a -- ^ Accumulator for each chunk.
       -> Array r ix e -> b
foldlP g !tAcc f = ifoldlP g tAcc (\ x _ -> f x)
{-# INLINE foldlP #-}


-- | /O(n)/ - Left fold with an index aware function, computed in parallel. Just
-- like `foldlP`, except that folding function will receive an index of an
-- element it is being applied to.
ifoldlP :: Source r ix e =>
           (b -> a -> b) -> b -> (a -> ix -> e -> a) -> a -> Array r ix e -> b
ifoldlP g !tAcc f !initAcc !arr = unsafePerformIO $ do
  let !sz = size arr
  results <- splitWork sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
    jId <-
      loopM 0 (< slackStart) (+ chunkLength) 0 $ \ !start !jId -> do
        submitRequest scheduler $
          JobRequest jId $
          iterLinearM sz start (start + chunkLength) 1 (<) initAcc $ \ !i ix !acc ->
            return $ f acc ix (unsafeLinearIndex arr i)
        return (jId + 1)
    when (slackStart < totalLength) $
      submitRequest scheduler $
      JobRequest jId $
      iterLinearM sz slackStart totalLength 1 (<) initAcc $ \ !i ix !acc ->
        return $ f acc ix (unsafeLinearIndex arr i)
  return $ F.foldl' g tAcc $ map jobResult $ sortOn jobResultId results
{-# INLINE ifoldlP #-}


-- | /O(n)/ - Left fold, computed in parallel. Same as `foldlP`, except directed
-- from the last element in the array towards beginning.
foldrP :: Source r ix e =>
          (a -> b -> b) -> b -> (e -> a -> a) -> a -> Array r ix e -> b
foldrP g !tAcc f = ifoldrP g tAcc (const f)
{-# INLINE foldrP #-}


-- | /O(n)/ - Right fold with an index aware function, computed in parallel.
-- Same as `ifoldlP`, except directed from the last element in the array towards
-- beginning.
ifoldrP :: Source r ix e =>
           (a -> b -> b) -> b -> (ix -> e -> a -> a) -> a -> Array r ix e -> b
ifoldrP g !tAcc f !initAcc !arr = unsafePerformIO $ do
  let !sz = size arr
  results <- splitWork sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
    when (slackStart < totalLength) $
      submitRequest scheduler $
      JobRequest 0 $
      iterLinearM sz (totalLength - 1) slackStart (-1) (>=) initAcc $ \ !i ix !acc ->
        return $ f ix (unsafeLinearIndex arr i) acc
    loopM slackStart (> 0) (subtract chunkLength) 1 $ \ !start !jId -> do
      submitRequest scheduler $
        JobRequest jId $
        iterLinearM sz (start - 1) (start - chunkLength) (-1) (>=) initAcc $ \ !i ix !acc ->
          return $ f ix (unsafeLinearIndex arr i) acc
      return (jId + 1)
  return $ F.foldr' g tAcc $ reverse $ map jobResult $ sortOn jobResultId results
{-# INLINE ifoldrP #-}




-- | /O(n)/ - Unstructured fold, computed in parallel.
foldP :: Source r ix e =>
         (e -> e -> e) -> e -> Array r ix e -> e
foldP f !initAcc = foldlP f initAcc f initAcc
{-# INLINE foldP #-}


-- | /O(n1 + n2)/ - Compute equality in parallel.
eqP :: (Eq a, Source r1 ix a, Source r2 ix a) =>
       Array r1 ix a -> Array r2 ix a -> Bool
eqP !arr1 !arr2 = (size arr1 == size arr2) && foldP (&&) True (M.zipWith (==) arr1 arr2)
{-# INLINE eqP #-}


-- | /O(n)/ - Compute sum in parallel.
sumP :: (Source r ix e, Num e) =>
        Array r ix e -> e
sumP = foldP (+) 0
{-# INLINE sumP #-}


-- | /O(n)/ - Compute product in parallel.
productP :: (Source r ix e, Num e) =>
            Array r ix e -> e
productP = foldP (*) 1
{-# INLINE productP #-}

