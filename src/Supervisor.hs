{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Supervisor where

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map

import StateMachine
import Codec

------------------------------------------------------------------------

data Supervisor = Supervisor
  { sChildren        :: [(Name, SomeSM)]
  , sRestartStrategy :: RestartStrategy
  }
  deriving (Generic, NFData)

data RestartStrategy = OneForOne | OneForAll | OneForRest
  deriving (Generic, NFData)

defaultGraceTimeMs :: Int
defaultGraceTimeMs = 500

------------------------------------------------------------------------

lookupSM :: Name -> Supervisor -> SomeSM
lookupSM name sup =
  case lookup name (sChildren sup) of
    Nothing  -> error ("lookupSM: no state machine with name: " ++ show name)
    Just ssm -> ssm

updateSM :: Name -> SomeSM -> Supervisor -> Supervisor
updateSM name ssm sup = sup { sChildren = update name ssm (sChildren sup) }
  where
    update :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
    update k v = map (\(k', v') -> if k == k' then (k, v) else (k', v'))

------------------------------------------------------------------------

step :: Name -> ByteString -> Supervisor -> (Supervisor, Either StepError ByteString)
step name bs sup = case lookupSM name sup of
  ssm@(SomeSM _ _ codec _ _) -> case cDecode codec bs of
    Nothing -> (sup, Left (DecodeError bs))
    Just i  ->
      let
        (ssm', eo) = stepSM i ssm
      in
        case eo of
          Left err -> (sup, Left err)
          Right o  -> (updateSM name ssm' sup, Right (cEncode codec o))

start :: Supervisor -> IO Supervisor
start sup = do
  children' <- mapM (\(name, ssm) -> startSMInit name ssm >>= \ssm' ->
                                     return (name, ssm'))
                    (sChildren sup)
  return sup { sChildren = children' }

restart :: Name -> Supervisor -> IO Supervisor
restart nameOfFailedSM sup = case sRestartStrategy sup of
  OneForOne -> do
    ssm' <- restartSM nameOfFailedSM defaultGraceTimeMs (lookupSM nameOfFailedSM sup)
    return (updateSM nameOfFailedSM ssm' sup)
  OneForAll -> do
    children' <- mapM (\(name, ssm) -> restartSM name defaultGraceTimeMs ssm >>= \ssm' ->
                                       return (name, ssm'))
                      (sChildren sup)
    return sup { sChildren = children' }
  OneForRest -> do
    children' <- mapM (\(name, ssm) -> restartSM name defaultGraceTimeMs ssm >>= \ssm' ->
                                       return (name, ssm'))
                      (rest nameOfFailedSM sup)
    return sup { sChildren = children' }
    where
      rest :: Name -> Supervisor -> [(Name, SomeSM)]
      rest name = dropWhile ((/= name) . fst) . sChildren
