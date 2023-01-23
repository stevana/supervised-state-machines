{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StateMachine where

import Control.Exception
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Typeable
import System.Timeout
import Control.DeepSeq

import Codec

------------------------------------------------------------------------

newtype Name = Name { unName :: String }
  deriving (Eq, Ord, Show, IsString)

newtype SM s i o = SM { runSM :: i -> s -> (s, o) }

data SomeSM = forall s i o. (Typeable s, Typeable i, Typeable o, NFData s, NFData o) => SomeSM
  { ssmSM        :: SM s i o
  , ssmState     :: s
  , ssmCodec     :: Codec i o
  , ssmInit      :: Name -> IO s
  , ssmTerminate :: Name -> s -> IO ()
  }

data StepError
  = InputTypeMismatch
  | OutputTypeMismatch
  | Exception SomeException
  | DecodeError ByteString
  deriving Show

instance Exception StepError

stepSM :: (Typeable i, Typeable o) => i -> SomeSM -> IO (SomeSM, Either StepError o)
stepSM mi ssm@(SomeSM (SM f) s _codec _init _stop) =
  case cast mi of
    Nothing -> return (ssm, Left InputTypeMismatch)
    Just i  -> do
      r <- try (evaluate (force (f i s)))
      case r of
        Left err       -> return (ssm, Left err)
        Right (s', mo) ->
          case cast mo of
            Nothing -> return (ssm, Left OutputTypeMismatch)
            Just o  -> return ((SomeSM (SM f) s' _codec _init _stop), Right o)

startSMInit :: Name -> SomeSM -> IO SomeSM
startSMInit name (SomeSM _f _s _codec init _stop) = do
  ms <- init name
  case cast ms of
    Just s  -> return (SomeSM _f s _codec init _stop)
    Nothing -> error "startSMInit: state type mismatch"

startSM :: Typeable s => Name -> s -> SomeSM -> IO SomeSM
startSM name ms (SomeSM _f _s _codec init _stop) = do
  _s <- init name
  case cast ms of
    Just s  -> return (SomeSM _f s _codec init _stop)
    Nothing -> error "startSM: state type mismatch"

stopSM :: Name -> Int -> SomeSM -> IO ()
stopSM name graceTimeMs (SomeSM _f ms _codec _init terminate) = do
  _ <- timeout (graceTimeMs * 1000) (terminate name ms)
  return ()

restartSM :: Name -> Int -> SomeSM -> IO SomeSM
restartSM name graceTimeMs ssm@(SomeSM _f s _codec _init terminate) = do
  stopSM name graceTimeMs ssm
  startSM name s ssm
