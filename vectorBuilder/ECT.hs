{-# LANGUAGE RankNTypes #-}

module ECT (
  ECT(..),
  ectYield,
  --
  ECTResult(..),
  runECT,
  runForceECT,
  --
  ECConcreteT(..),
  ECStep(..),
  runForceECConcreteT
  ) where

import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype ECT o m a =
  EC { unEC :: forall r. (a -> m r) -> (o -> m r -> m r) -> m r }

instance Monad m => Functor (ECT o m) where
  fmap f (EC eca) = EC (\kv ke -> eca (kv . f) ke)
  {-# INLINE fmap #-}

instance Monad m => Applicative (ECT o m) where
  pure v = EC (\kv _ -> kv v)
  {-# INLINE pure #-}
  (EC ecf) <*> (EC eca) = EC (\kv ke -> ecf (\f -> eca (\a -> kv (f a)) ke) ke)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (ECT o m) where
  return = pure
  {-# INLINE return #-}
  (EC eca) >>= f = EC (\kv ke -> eca (\a -> unEC (f a) kv ke) ke)
  {-# INLINE (>>=) #-}

instance MonadTrans (ECT o) where
  lift m = EC (\kv _ -> m >>= kv)

instance MonadIO m => MonadIO (ECT o m) where
  liftIO = lift . liftIO

ectYield :: Monad m => o -> ECT o m ()
ectYield o = EC (\kv ke -> ke o (kv ()))



--------------------------------------------------------------------------------
-- Everything from down here is only for testing/playing with!

newtype ECTResult o m a = ECR { runECTResult :: m (Either (o, ECTResult o m a) a) }

runECT :: Monad m => ECT o m a -> m (Either (o, ECTResult o m a) a)
runECT (EC eca) = eca (return . Right) (\o k -> return $ Left (o, (ECR k)))

runForceECT :: Monad m => ECT o m a -> m a
runForceECT (EC eca) = eca return (error "ECT exception")
{-# INLINE runForceECT #-}

--------------------------------------------------------------------------------

newtype ECConcreteT o m a = ECConcrete { runECConcrete :: m (ECStep o m a) }

data ECStep o m a = ECPure a
                  | ECYield o (ECConcreteT o m a)

instance Monad m => Functor (ECConcreteT o m) where
  fmap f = ECConcrete . liftM (fmap f) . runECConcrete

instance Monad m => Functor (ECStep o m) where
  fmap f = \s -> case s of
    ECPure a -> ECPure (f a)
    ECYield o k -> ECYield o (fmap f k)

instance Monad m => Applicative (ECConcreteT o m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ECConcreteT o m) where
  return v = ECConcrete $ return $ ECPure v
  {-# INLINE return #-}
  (ECConcrete ma) >>= f = ECConcrete $ do
    sa <- ma
    case sa of
      ECPure a -> runECConcrete (f a)
      ECYield o k -> return $ ECYield o (k >>= f)
  {-# INLINE (>>=) #-}

instance MonadTrans (ECConcreteT o) where
  lift m = ECConcrete $ liftM ECPure m
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (ECConcreteT o m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

runForceECConcreteT :: Monad m => ECConcreteT o m a -> m a
runForceECConcreteT ma = do
  sa <- runECConcrete ma
  case sa of
    ECPure a -> return a
    ECYield _ _ -> fail "ECConcreteT yield"
