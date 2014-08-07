module VectorBuilder (
  vectorBuilder'
  ) where

import           Control.Applicative
import           Control.Monad               (when, liftM)
import           Control.Monad.Trans.Class
import           Data.Primitive.MutVar       (MutVar, newMutVar, readMutVar,
                                              writeMutVar)
import           Control.Monad.Base
import           Control.Monad.Primitive     (PrimState, PrimMonad)
import           Data.Conduit
import           Data.Conduit.Internal       (ConduitM (..), Pipe (..))
import qualified Data.Conduit.Internal       as CI
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import           Data.Void                   (absurd)

import           ECT

vectorBuilder' :: (PrimMonad base,
                   MonadBase base m,
                   V.Vector v e)
                  => Int -- ^ size
                  -> ((e -> ECT (v e) base ()) -> Sink i (ECT (v e) base) r)
                  -> ConduitM i (v e) m r
vectorBuilder' size inner = do
    ref <- liftBase $ do
        mv <- VM.new size
        newMutVar $! S 0 mv
    res <- liftInnerYields (inner (addE ref))
    S idx mv <- liftBase $ readMutVar ref
    when (idx > 0) $ do
      v <- liftBase $ V.unsafeTake idx <$> V.unsafeFreeze mv
      yield v
    return res
{-# INLINE vectorBuilder' #-}

data S s v e = S
    {-# UNPACK #-} !Int -- index
    !(V.Mutable v s e)


addE :: (PrimMonad m, V.Vector v e)
     => MutVar (PrimState m) (S (PrimState m) v e)
     -> e
     -> ECT (v e) m ()
addE ref e = do
    S idx mv <- lift $ readMutVar ref
    lift $ VM.write mv idx e
    let idx' = succ idx
        size = VM.length mv
    if idx' >= size
        then do
            v <- lift $ do mv' <- VM.new size
                           writeMutVar ref $! S 0 mv'
                           V.unsafeFreeze mv
            ectYield v
        else lift $ writeMutVar ref $! S idx' mv
{-# INLINE addE #-}

liftInnerYields :: (MonadBase base m)
                   => Sink i (ECT o base) r
                   -> ConduitM i o m r
liftInnerYields = ConduitM . go . unConduitM
  where
    go (Done r) = Done r
    go (HaveOutput _ _ o) = absurd o
    go (NeedInput f g) = NeedInput (go . f) (go . g)
    go (PipeM mp) = PipeM (liftBase $ unEC mp (return . go)
                           (\o k -> (CI.yield o >>) `liftM` k))
    go (Leftover f i) = Leftover (go f) i

    -- Easier to understand:
    -- go (PipeM mp) = PipeM (liftBase . liftM go' . runECT $ mp)
    -- go' yp = case yp of
    --   Right p -> go p
    --   Left (o, rmp) -> CI.yield o >> PipeM (liftBase . liftM go' . runECTResult $ rmp)
{-# INLINE liftInnerYields #-}
