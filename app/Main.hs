{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM.TBQueue

import Example.KeyValueStore
import Supervisor
import EventLoop

------------------------------------------------------------------------

main :: IO ()
main = do
  let sup = Supervisor [("kv1", kvStore), ("kv2", kvStore), ("kv3", kvStore)] RestForOne
  queue <- newTBQueueIO 128
  withEventLoop sup queue $ do
    call_ "kv2" (Store "x" 1) queue
    call_ "kv2" (Lookup "crash") queue
    r1 <- call "kv2" (Lookup "x") queue
    print r1
    r2 <- call "kv2" (Lookup "y") queue
    print r2
