module L.L2.Interpreter.Interp where

import Control.Monad (foldM)
import Data.Map (Map)
import Data.Map qualified as Map

import L.L2.Frontend.Syntax

import Utils.Pretty
import Utils.Value
import Utils.Var

type Env = Map Var Value

interp :: L2 -> IO (Either String Env)
interp = evalL2

evalL2 :: L2 -> IO (Either String Env)
evalL2 (L2 ss) =
  foldM step (Right Map.empty) ss
 where
  step ac@(Left _) _ = pure ac
  step (Right env) s2 = evalS2 env s2

evalS2 :: Env -> S2 -> IO (Either String Env)
evalS2 env (LRead s v) =
  do
    putStr s
    val <- readValue
    pure (Right $ Map.insert v val env)
evalS2 env (LPrint e) =
  case evalE2 env e of
    Left err -> pure $ Left err
    Right val -> do
      putStrLn (pretty val)
      pure (Right env)
evalS2 env (LAssign v e) =
  case evalE2 env e of
    Left err -> pure $ Left err
    Right val -> pure (Right $ Map.insert v val env)
evalS2 env (Def v e ss) =
  case evalE2 env e of
    Left err -> pure $ Left err
    Right val -> do
      let originalEnv = env
      let env' = Map.insert v val env
      result <- foldM stepDef (Right env') ss
      case result of
        Left err -> pure $ Left err
        Right _ -> pure (Right originalEnv)
 where
  stepDef acc@(Left _) _ = pure acc
  stepDef (Right env) s = evalS2 env s

readValue :: IO Value
readValue = (VInt . read) <$> getLine

evalE2 :: Env -> E2 -> Either String Value
evalE2 _ (LVal v) = Right v
evalE2 env (LVar v) =
  case Map.lookup v env of
    Just val -> Right val
    Nothing -> Left ("Undefined Variable" ++ pretty v)
evalE2 env (LAdd l1 l2) =
  do
    v1 <- evalE2 env l1
    v2 <- evalE2 env l2
    v1 .+. v2
evalE2 env (LMul l1 l2) =
  do
    v1 <- evalE2 env l1
    v2 <- evalE2 env l2
    v1 .*. v2
evalE2 env (LDiv l1 l2) =
  do
    v1 <- evalE2 env l1
    v2 <- evalE2 env l2
    v1 ./. v2
evalE2 env (LMinus l1 l2) =
  do
    v1 <- evalE2 env l1
    v2 <- evalE2 env l2
    v1 .-. v2
