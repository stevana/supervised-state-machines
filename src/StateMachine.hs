{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module StateMachine where

import Data.String (IsString)
import Data.Typeable
import System.Timeout

import Codec

------------------------------------------------------------------------

newtype Name = Name { unName :: String }
  deriving (Eq, Ord, Show, IsString)

newtype SM s i o = SM { runSM :: i -> s -> (s, o) }

data SomeSM = forall s i o. (Typeable s, Typeable i, Typeable o) => SomeSM
  { ssmSM        :: SM s i o
  , ssmState     :: s
  , ssmCodec     :: Codec i o
  , ssmInit      :: IO s
  , ssmTerminate :: s -> IO ()
  }

stepSM :: (Typeable i, Typeable o) => i -> SomeSM -> (SomeSM, o)
stepSM mi (SomeSM (SM f) s _codec _init _stop) =
  case cast mi of
    Nothing -> error "stepSM: input type mismatch"
    Just i  ->
      let
        (s', mo) = f i s
      in
        case cast mo of
          Nothing -> error "stepSM: output type mismatch"
          Just o  -> (SomeSM (SM f) s' _codec _init _stop, o)

startSM :: Name -> SomeSM -> IO SomeSM
startSM _name (SomeSM _f _s _codec init _stop) = do
  ms <- init
  case cast ms of
    Just s  -> return (SomeSM _f s _codec init _stop)
    Nothing -> error "startSM: state type mismatch"

stopSM :: Name -> Int -> SomeSM -> IO ()
stopSM name graceTimeMs (SomeSM _f s _codec _init stop) = do
  _ <- timeout (graceTimeMs * 1000) (stop s)
  return ()

restartSM :: Name -> Int -> SomeSM -> IO SomeSM
restartSM name graceTimeMs ssm = do
  stopSM name graceTimeMs ssm
  startSM name ssm
