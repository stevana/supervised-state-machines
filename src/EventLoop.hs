{-# LANGUAGE ScopedTypeVariables #-}

module EventLoop where

import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
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
          r <- try (evaluate (step name bs sup))
          case r of
            Left (err :: SomeException) -> do
              putStrLn "exception"
              putMVar response (BS8.pack (displayException err))
              putStrLn (displayException err)
              sup' <- restart name sup
              go sup' queue
            Right (sup', o) -> do
              putMVar response o
              go sup' queue

withEventLoop :: Application -> TBQueue Event -> IO a -> IO a
withEventLoop sup queue k = withAsync (eventLoop sup queue) $ \a -> do
  link a
  k

call :: (Show i, Read o) => Name -> i -> TBQueue Event -> IO o
call name i queue = do
  response <- newEmptyMVar
  atomically (writeTBQueue queue (Input name (BS8.pack (show i)) response))
  mr <- timeout 1000 (takeMVar response)
  case mr of
    Nothing -> error "call: timed out"
    Just r  -> case readMaybe (BS8.unpack r) of
      Nothing -> error "call: couldn't read output"
      Just o  -> return o
