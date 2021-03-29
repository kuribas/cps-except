{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.CPSExcept where
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader()
import Control.Monad.Writer()
import Control.Monad.RWS()

newtype CPSExceptT e m a =
  CPSExceptT { getCPSExceptT :: forall r. ((e -> m r) -> (a -> m r) -> m r) }

runCPSExceptT :: Applicative m => CPSExceptT e m a -> m (Either e a)
runCPSExceptT (CPSExceptT f) = f (pure . Left)  (pure . Right)
{-# INLINE runCPSExceptT #-}

mapCPSExceptT :: (Monad m, Monad n) =>
                 (m (Either e a) -> n (Either e' b)) -> CPSExceptT e m a
              -> CPSExceptT e' n b
mapCPSExceptT f m = CPSExceptT $ \failC succesC ->
  either failC succesC =<< f (runCPSExceptT m)

withExceptT :: Functor m => (e -> e') -> CPSExceptT e m a -> CPSExceptT e' m a
withExceptT tr (CPSExceptT f) = CPSExceptT $ \failC succesC ->
  f (failC . tr) succesC

instance Functor (CPSExceptT e m) where
  fmap f (CPSExceptT g) = CPSExceptT $ \failC successC ->
    g failC (successC . f)
  {-# INLINE fmap #-}

instance Monad m => Applicative (CPSExceptT e m) where
  pure x = CPSExceptT $ \_failC successC -> successC x
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (CPSExceptT e m) where
  CPSExceptT f >>= g = CPSExceptT $ \failC successC ->
    f failC (\a -> getCPSExceptT (g a) failC successC)
  {-# INLINE (>>=) #-}

instance Monad m => MonadError e (CPSExceptT e m) where
  throwError e = CPSExceptT $ \failC _successC -> failC e
  {-# INLINE throwError #-}
  catchError (CPSExceptT f) handler =
    CPSExceptT $ \failC successC ->
    f (\e -> getCPSExceptT (handler e) failC successC)
    successC
  {-# INLINE catchError #-}

instance MonadTrans (CPSExceptT e) where
  lift m = CPSExceptT $ \_failC successC -> m >>= successC
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (CPSExceptT e m) where
  state f = lift $ state f
  {-# INLINE state #-}

  
    
