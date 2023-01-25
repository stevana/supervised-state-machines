{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module StateMachine where

import Control.DeepSeq (NFData, rnf)
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import System.Timeout (timeout)

import Codec

------------------------------------------------------------------------

newtype Name = Name { unName :: String }
  deriving newtype (Eq, Ord, Show, IsString)
  deriving stock Generic
  deriving anyclass NFData

newtype SM s i o = SM { runSM :: i -> s -> (s, o) }

data SomeSM = forall s i o. (Typeable s, Typeable i, Typeable o, NFData s, NFData o) => SomeSM
  { ssmSM        :: SM s i o
  , ssmState     :: s
  , ssmCodec     :: Codec i o
  , ssmInit      :: Name -> IO s
  , ssmTerminate :: Name -> s -> IO ()
  }

instance NFData SomeSM where
  rnf (SomeSM _f s _codec _init _terminate) = rnf s

data StepError
  = InputTypeMismatch
  | OutputTypeMismatch
  | DecodeError ByteString
  deriving stock (Show, Generic)
  deriving anyclass NFData

stepSM :: (Typeable i, Typeable o) => i -> SomeSM -> Either StepError (SomeSM, o)
stepSM mi ssm@(SomeSM (SM f) s _codec _init _terminate) =
  case cast mi of
    Nothing -> Left InputTypeMismatch
    Just i  ->
      let
        (s', mo) = f i s
      in
        case cast mo of
          Nothing -> Left OutputTypeMismatch
          Just o  -> Right (SomeSM (SM f) s' _codec _init _terminate, o)

startSM :: Name -> SomeSM -> IO SomeSM
startSM name (SomeSM _f _s _codec init _stop) = do
  ms <- init name
  case cast ms of
    Just s  -> return (SomeSM _f s _codec init _stop)
    Nothing -> error "startSMI: state type mismatch"

stopSM :: Name -> Int -> SomeSM -> IO ()
stopSM name graceTimeMs (SomeSM _f ms _codec _init terminate) = do
  _ <- timeout (graceTimeMs * 1000) (terminate name ms)
  return ()

restartSM :: Name -> Int -> SomeSM -> IO SomeSM
restartSM name graceTimeMs ssm@(SomeSM _f _s _codec _init terminate) = do
  stopSM name graceTimeMs ssm
  startSM name ssm
