{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module EventLoop where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.DeepSeq
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import System.Timeout
import Text.Read (readMaybe)

import StateMachine
import Supervisor

------------------------------------------------------------------------

data Event = Input Name ByteString (MVar ByteString)

data EventLoop = EventLoop
  { elQueue :: TBQueue Event
  }

type Application = Supervisor

eventLoop :: Application -> TBQueue Event -> IO ()
eventLoop sup0 queue = do
  sup <- start sup0
  go sup queue
  where
    go :: Supervisor -> TBQueue Event -> IO ()
    go sup queue = do
      e <- atomically (readTBQueue queue)
      case e of
        Input name bs response -> do
          r <- try (evaluate (force (step name bs sup)))
          case r of
            Left (err :: SomeException) -> do
              -- putMVar response (BS8.pack (displayException err))
              putStrLn (unName name ++ " threw: " ++ displayException err)
              sup' <- restart name sup
              go sup' queue
            Right (Left err) -> do
              print err
              go sup queue
            Right (Right (sup', o)) -> do
              putMVar response o
              go sup' queue

withEventLoop :: Application -> TBQueue Event -> IO a -> IO a
withEventLoop sup queue k = withAsync (eventLoop sup queue) $ \a -> do
  link a
  k

data CallError = CallTimeout
  deriving Show

call :: Show i => Name -> i -> TBQueue Event -> IO (Either CallError String)
call name i queue = do
  putStrLn ("Calling " ++ unName name ++ ": " ++ show i)
  response <- newEmptyMVar
  atomically (writeTBQueue queue (Input name (BS8.pack (show i)) response))
  mr <- timeout 1000 (takeMVar response)
  case mr of
    Nothing -> return (Left CallTimeout)
    Just r  -> return (Right (BS8.unpack r))

call_ :: (Show i) => Name -> i -> TBQueue Event -> IO ()
call_ name i queue = do
  _ <- call name i queue
  return ()
