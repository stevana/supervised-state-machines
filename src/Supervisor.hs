{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Supervisor where

import Control.Applicative
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)

import Codec
import StateMachine

------------------------------------------------------------------------

data SupervisorF a
  = Worker a
  | Supervisor RestartStrategy [SupervisorF a]
  deriving (Functor, Foldable, Traversable, Generic, NFData)

type Supervisor = SupervisorF (Name, SomeSM)

data RestartStrategy = OneForOne | OneForAll | RestForOne
  deriving (Generic, NFData)

supervisorOf :: Name -> Supervisor -> Maybe Supervisor
supervisorOf name = go Nothing
  where
    go :: Maybe Supervisor -> Supervisor -> Maybe Supervisor
    go sup (Worker (name', _ssm)) | name == name'  = sup
                                  | otherwise      = Nothing
    go _sup sup@(Supervisor _strategy children) = go' (Just sup) children

    go' :: Maybe Supervisor -> [Supervisor] -> Maybe Supervisor
    go' sup []       = Nothing
    go' sup (c : cs) = go sup c <|> go' sup cs

restartStrategy :: Name -> Supervisor -> RestartStrategy
restartStrategy name sup = case supervisorOf name sup of
  Nothing -> error "restartStrategy: invalid supervisor tree"
  Just (Supervisor strategy _children) -> strategy
  Just (Worker _) -> error "restartStrategy: impossilbe"

defaultGraceTimeMs :: Int
defaultGraceTimeMs = 500

------------------------------------------------------------------------

lookupSM :: Name -> Supervisor -> SomeSM
lookupSM name sup =
  case lookup name (toList sup) of
    Nothing  -> error ("lookupSM: no state machine with name: " ++ show name)
    Just ssm -> ssm

updateSM :: Name -> SomeSM -> Supervisor -> Supervisor
updateSM name ssm = go
  where
    go :: Supervisor -> Supervisor
    go w@(Worker (name', _ssm)) | name == name' = Worker (name', ssm)
                                | otherwise     = w
    go (Supervisor strategy children) = Supervisor strategy (map go children)

------------------------------------------------------------------------

step :: Name -> ByteString -> Supervisor -> Either StepError (Supervisor, ByteString)
step name bs sup = case lookupSM name sup of
  ssm@(SomeSM _ _ codec _ _) -> case cDecode codec bs of
    Nothing -> Left (DecodeError bs)
    Just i  ->
      case stepSM i ssm of
        Left err        -> Left err
        Right (ssm', o) -> Right (updateSM name ssm' sup, cEncode codec o)

start :: Supervisor -> IO Supervisor
start = mapM (\(name, ssm) -> startSM name ssm >>= \ssm' ->
                              return (name, ssm'))

restart :: Name -> Supervisor -> IO Supervisor
restart _nameOfFailedSM (Worker _) = error "restart: invalid supervisor tree"
restart nameOfFailedSM sup = case restartStrategy nameOfFailedSM sup of
  OneForOne -> do
    ssm' <- restartSM nameOfFailedSM defaultGraceTimeMs (lookupSM nameOfFailedSM sup)
    return (updateSM nameOfFailedSM ssm' sup)
  OneForAll -> do
    mapM_ (\(name, ssm) -> stopSM name defaultGraceTimeMs ssm) sup
    mapM (\(name, ssm) -> startSM name ssm >>= \ssm' ->
                          return (name, ssm'))
         sup
  RestForOne -> do
    let restChildren = rest nameOfFailedSM sup
    mapM_ (\(name, ssm) -> stopSM name defaultGraceTimeMs ssm >>= \ssm' ->
                           return (name, ssm'))
          restChildren
    mapM (\(name, ssm) -> startSM name ssm >>= \ssm' ->
                          return (name, ssm'))
         restChildren
    where
      rest :: Name -> Supervisor -> Supervisor
      rest name sup = case supervisorOf name sup of
        Nothing -> error "rest: invalid supervisor tree"
        Just (Supervisor strategy children) -> Supervisor strategy (go children)
          where
            go :: [Supervisor] -> [Supervisor]
            go [] = []
            go (c@(Worker (name', _ssm)) : cs) | name == name' = c : cs
                                               | otherwise     = go cs
