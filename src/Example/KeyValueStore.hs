module Example.KeyValueStore where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import qualified Data.Map as Map
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
  deriving (Show, Read)

smKVStore :: SM (Map String Int) Input Output
smKVStore = SM f
  where
    f :: Input -> Map String Int -> (Map String Int, Output)
    f (Store k v)       s = (Map.insert k v s, Ok)
    -- f (Lookup "crash")  s = (s, Result (Just (1 `div` 0))) -- Deliberate error.
    f (Lookup "crash")  s = (s, Result (Just (error "hi"))) -- Deliberate error.
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

initKVStore :: IO (Map String Int)
initKVStore = do
  putStrLn "KV store starting"
  return initStateKVStore

terminateKVStore :: Map String Int -> IO ()
terminateKVStore _s = putStrLn "KV store terminating"

kvStore :: SomeSM
kvStore = SomeSM smKVStore initStateKVStore codecKVStore initKVStore terminateKVStore
