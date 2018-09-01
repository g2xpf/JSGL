{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module JSGL.Types.GS (
  GS,
  runGS,
  sync
) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar, MVar)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (runStateT, StateT)
import Control.Monad.Trans.Class
import Control.Monad.State (MonadState(..))
import Control.Monad.State.Class
import Control.Monad.Reader (MonadReader(..))
import Control.Applicative

newtype CState s r m a = CState { runCState :: (r, s) -> m (a, s) }

instance (Functor m) => Functor (CState s r m) where
  fmap f m = CState $ \(r, s) -> 
    fmap (\ ~(a, s') -> (f a, s')) $ runCState m (r, s)

instance (Functor m, Monad m) => Applicative (CState s r m) where
  pure m = CState $ \(r, s) -> return (m, s)
  f <*> m = do
    f' <- f
    CState $ \(r, s) -> do
      (a, s') <- runCState m (r, s)
      return (f' a, s')

instance (Monad m) => Monad (CState s r m) where
  return a = CState $ \(r, s) -> return (a, s)
  {-# INLINE return #-}

  m >>= f = CState $ \(r, s) -> do
              (a, s') <- runCState m (r, s)
              runCState (f a) (r, s')
  {-# INLINE (>>=) #-}

  fail str = CState $ \_ -> fail str
  {-# INLINE fail #-}

instance Monad m => MonadState s (CState s r m) where
  get = CState $ \(r, s) -> return (s, s)
  put s = CState $ \_ -> return ((), s)

instance Monad m => MonadReader r (CState s r m) where
  reader f = CState $ \(r, s) -> return (f r, s)
  local f m = CState $ \(r, s) -> runCState m (f r, s)

instance MonadTrans (CState s r) where
  lift m = CState $ \(r, s) -> do
    val <- m
    return (val, s)

instance MonadIO m => MonadIO (CState s r m) where
  liftIO =  lift . liftIO

type GS s a = CState s Synchronizer IO a

data Synchronizer = Synchronizer {
  starter :: MVar (),
  stopper :: MVar () 
}

runGS :: GS s a -> Int -> s -> IO (a, s)
runGS gs interval initState = do
  synchronizer <- getSynchronizer
  measureOn interval synchronizer
  runCState gs (synchronizer, initState)

sync :: GS s ()
sync = do
  Synchronizer sync1 sync2 <- ask
  liftIO $ do
    putMVar sync2 ()
    takeMVar sync1

getSynchronizer :: IO Synchronizer
getSynchronizer = do
  sync1 <- newEmptyMVar
  sync2 <- newEmptyMVar
  return $ Synchronizer sync1 sync2
  
measureOn :: Int -> Synchronizer -> IO ()
measureOn interval (Synchronizer sync1 sync2) = do
  forkIO . forever $ do
    threadDelay interval
    putMVar sync1 ()
    takeMVar sync2
  return ()
