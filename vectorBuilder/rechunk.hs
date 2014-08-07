{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           ClassyPrelude.Conduit
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.ST        (runST)
import           Criterion.Main          (bench, bgroup, defaultMain, nfIO,
                                          whnfIO)
import qualified System.Random.MWC       as MWC
import qualified Data.Vector.Generic     as V
import           Test.Hspec              (hspec, shouldBe)
import           Test.Hspec.QuickCheck   (prop)

import           VectorBuilder

rechunk1 :: ( Monad m
            , V.Vector vector (Element input)
            , PrimMonad base
            , MonadBase base m
            , MonoFoldable input
            )
         => Conduit input m (vector (Element input))
rechunk1 = concatC =$= concatMapC (\x -> [x, x]) =$= conduitVector 512
{-# INLINE rechunk1 #-}

rechunk2 :: (Monad m, IsSequence a) => Conduit a m a
rechunk2 =
    mapC (concatMap $ replicate 2) =$= loop
  where
    loop = do
        x <- takeCE 512 =$= foldC
        unless (null x) $ yield x >> loop
{-# INLINE rechunk2 #-}

rechunk3 :: ( MonadBase base m
            , PrimMonad base
            , MonoFoldable input
            , V.Vector vector (Element input)
            )
         => Conduit input m (vector (Element input))
rechunk3 = vectorBuilderC 5120 $ \yield' ->
    mapM_CE (\x -> yield' x >> yield' x)
{-# INLINE rechunk3 #-}

rechunk4 :: ( MonadBase base m
            , PrimMonad base
            , MonoFoldable input
            , V.Vector vector (Element input)
            )
         => Conduit input m (vector (Element input))
rechunk4 = vectorBuilder' 5120 $ \yield' ->
    mapM_CE (\x -> yield' x >> yield' x)
{-# INLINE rechunk4 #-}


main :: IO ()
main = do
    hspec $ prop "rechunking" $ \ws -> do
        let src = yield (pack ws :: UVector Word8)
            doubled = concatMap (\w -> [w, w]) ws
            res1 = runST $ src $$ rechunk1 =$ sinkList
            res2 = runST $ src $$ rechunk2 =$ sinkList
            res3 = runST $ src $$ rechunk3 =$ sinkList
            res4 = runST $ src $$ rechunk4 =$ sinkList
        res1 `shouldBe` (res2 :: [UVector Word8])
        (res3 :: [UVector Word8]) `shouldBe` (res2 :: [UVector Word8])
        (res4 :: [UVector Word8]) `shouldBe` (res2 :: [UVector Word8])
        (unpack $ (mconcat res2 :: UVector Word8)) `shouldBe` (doubled :: [Word8])
        case reverse res2 :: [UVector Word8] of
            [] -> return ()
            x:xs -> do
                (length x <= 512) `shouldBe` True
                all ((== 512) . length) xs `shouldBe` True

    gen <- MWC.createSystemRandom
    bytes <- replicateM 20 $
        MWC.uniformR (12, 102400) gen >>= MWC.uniformVector gen

    defaultMain
        [ bgroup "copy bytes"
            -- [ bench "rechunk1" $ whnfIO
            --     $ yieldMany (bytes :: [UVector Word8])
            --    $$ (rechunk1 :: Conduit (UVector Word8) IO (UVector Word8))
            --    =$ sinkNull
            -- , bench "rechunk2" $ whnfIO
            --     $ yieldMany (bytes :: [UVector Word8])
            --    $$ (rechunk2 :: Conduit (UVector Word8) IO (UVector Word8))
            --    =$ sinkNull
            [ bench "rechunk3" $ whnfIO
                $ yieldMany (bytes :: [UVector Word8])
               $$ (rechunk3 :: Conduit (UVector Word8) IO (UVector Word8))
               =$ sinkNull
            , bench "rechunk4" $ whnfIO
                $ yieldMany (bytes :: [UVector Word8])
               $$ (rechunk4 :: Conduit (UVector Word8) IO (UVector Word8))
               =$ sinkNull
            ]
        , bgroup "transformers" $
            let src = return () in
            [ bench "single" $ nfIO $ do
                ref <- newIORef (0 :: Int)
                let incr = modifyIORef' ref succ
                src $$ liftIO (replicateM_ 1000 incr)
            , bench "multi" $ nfIO $ do
                ref <- newIORef (0 :: Int)
                let incr = liftIO $ modifyIORef' ref succ
                src $$ replicateM_ 1000 incr
            ]
        ]
