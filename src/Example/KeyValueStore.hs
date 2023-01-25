{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Example.KeyValueStore where

import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Text.Read

import Codec
import StateMachine

------------------------------------------------------------------------

data Input
  = Store String Int
  | Lookup String
  deriving (Show, Read)

data Output
  = Ok
  | Result (Maybe Int)
  deriving (Show, Read, NFData, Generic)

smKVStore :: SM (Map String Int) Input Output
smKVStore = SM f
  where
    f :: Input -> Map String Int -> (Map String Int, Output)
    f (Store k v)       s = (Map.insert k v s, Ok)
    f (Lookup "crash")  s = (s, Result (Just (1 `div` 0))) -- Deliberate error.
    f (Lookup k)        s = (s, Result (Map.lookup k s))

initStateKVStore :: Map String Int
initStateKVStore = Map.empty

codecKVStore :: Codec Input Output
codecKVStore = Codec decode encode
  where
    decode :: ByteString -> Maybe Input
    decode = readMaybe . BS8.unpack

    encode :: Output -> ByteString
    encode = BS8.pack . show

initKVStore :: Name -> IO (Map String Int)
initKVStore name = do
  putStrLn ("KV store starting: " ++ unName name)
  return initStateKVStore

terminateKVStore :: Name -> Map String Int -> IO ()
terminateKVStore name _s = putStrLn ("KV store terminating: " ++ unName name)

kvStore :: SomeSM
kvStore = SomeSM smKVStore initStateKVStore codecKVStore initKVStore terminateKVStore
