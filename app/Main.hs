{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM.TBQueue

import Example.KeyValueStore
import Supervisor
import EventLoop

------------------------------------------------------------------------

main :: IO ()
main = do
  let sup = Supervisor [("kv1", kvStore), ("kv2", kvStore)] OneForOne
  queue <- newTBQueueIO 128
  withEventLoop sup queue $ do
    call "kv1" (Store "x" 1) queue :: IO Output
    call "kv1" (Lookup "crash") queue :: IO Output
    r1 <- call "kv1" (Lookup "x") queue :: IO Output
    print r1
    r2 <- call "kv1" (Lookup "y") queue :: IO Output
    print r2
