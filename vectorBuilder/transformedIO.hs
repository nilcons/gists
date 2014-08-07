{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Criterion.Main
import Data.Conduit (($$))
import Data.Conduit.Internal (runPipe)
import Data.Maybe
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Pipes (runEffect, (>->))

import ECT

n :: Int
n = 200000

for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !a !b op = go a
  where
    go k | k >= b    = return ()
         | otherwise = op k >> go (k+1)

v :: V.Vector Int
v = V.generate n id

-- With this instead of 'doN' only IdentityT is unaffected!
forNVector :: Monad m => (Int -> m ()) -> m ()
forNVector op = V.forM_ v op

direct :: IOVector Int -> IOVector Int -> IO ()
{-# NOINLINE direct #-}
direct v1 v2 =
  for 0 n $ \i -> do
    x <- MV.unsafeRead v1 i
    MV.unsafeWrite v2 (2*i) x
    MV.unsafeWrite v2 (2*i + 1) x

transformed :: MonadIO m => IOVector Int -> IOVector Int -> m ()
{-# INLINE transformed #-}
transformed v1 v2 =
  for 0 n $ \i -> do
    x <- liftIO $ MV.unsafeRead v1 i
    liftIO $ MV.unsafeWrite v2 (2*i) x
    liftIO $ MV.unsafeWrite v2 (2*i + 1) x

identityT, maybeT, contT, codensity, ect, ect2, pipes, pipes2, conduit, conduit2
  :: IOVector Int -> IOVector Int -> IO ()

{-# NOINLINE identityT #-}
{-# NOINLINE maybeT #-}
{-# NOINLINE contT #-}
{-# NOINLINE codensity #-}
{-# NOINLINE ect #-}
{-# NOINLINE ect2 #-}
{-# NOINLINE pipes #-}
{-# NOINLINE pipes2 #-}
{-# NOINLINE conduit #-}
{-# NOINLINE conduit2 #-}

identityT v1 v2 = runIdentityT $ transformed v1 v2

maybeT v1 v2 = fmap fromJust $ runMaybeT $ transformed v1 v2

contT v1 v2 = flip runContT return $ transformed v1 v2

codensity v1 v2 = lowerCodensity $ transformed v1 v2

ect v1 v2 = runForceECT $ transformed v1 v2

ect2 v1 v2 = runForceECConcreteT $ transformed v1 v2

pipes v1 v2 = runEffect $ transformed v1 v2

pipes2 v1 v2 = runEffect $ return () >-> transformed v1 v2

conduit v1 v2 = runPipe $ transformed v1 v2

conduit2 v1 v2 = return () $$ transformed v1 v2


benchmarks :: IOVector Int -> IOVector Int -> [Benchmark]
benchmarks v1 v2 = [
  bench "direct" $ whnfIO $ direct v1 v2,
  bench "identity" $ whnfIO $ identityT v1 v2,
  bench "maybe" $ whnfIO $ maybeT v1 v2,
  bench "cont" $ whnfIO $ contT v1 v2,
  bench "codensity" $ whnfIO $ codensity v1 v2,
  bench "ect" $ whnfIO $ ect v1 v2,
  bench "ect2" $ whnfIO $ ect2 v1 v2,
  bench "pipes" $ whnfIO $ pipes v1 v2,
  bench "pipes2" $ whnfIO $ pipes2 v1 v2,
  bench "conduit" $ whnfIO $ conduit v1 v2,
  bench "conduit2" $ whnfIO $ conduit2 v1 v2
  ]

main :: IO ()
main = do
  v1 <- MV.replicate n 5
  v2 <- MV.replicate (2*n) 0
  defaultMain $ benchmarks v1 v2
