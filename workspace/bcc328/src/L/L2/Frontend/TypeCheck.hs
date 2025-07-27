module L.L2.Frontend.TypeCheck where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List ((\\))

import L.L2.Frontend.Syntax
import Utils.Var

-- typeCheck :: L2 -> Either String L2
-- typeCheck = error "Not implemented!"
--
--
typeCheck :: L2 -> Either String L2
typeCheck prog =
  let ((res, _logs), _finalEnv) = runTcM initTcEnv (checkL2 prog)
   in res

-- | Verifica todo o programa
checkL2 :: L2 -> TcM L2
checkL2 (L2 ss) = do
  ss' <- mapM checkS2 ss
  return (L2 ss')

-- | Verifica um comando S2, lançando erro se v for imutável
checkS2 :: S2 -> TcM S2
checkS2 stmt = case stmt of
  -- def v := e in block end
  Def v e block -> do
    e' <- checkE2 e -- 1) checa expressão
    insertVar v -- 2) marca v como imutável
    block' <- mapM checkS2 block
    removeVar v -- 3) sai do escopo
    return (Def v e' block')

  -- read(msg, v)
  LRead msg v -> do
    env <- get
    when (v `elem` context env) $
      throwError $
        "cannot read into immutable " ++ show v
    return (LRead msg v)

  -- print(e)
  LPrint e -> do
    e' <- checkE2 e
    return (LPrint e')

  -- v := e
  LAssign v e -> do
    env <- get
    when (v `elem` context env) $
      throwError $
        "cannot assign to immutable " ++ show v
    e' <- checkE2 e
    return (LAssign v e')

-- | Verifica expressão descendo recursivamente
checkE2 :: E2 -> TcM E2
checkE2 expr = case expr of
  LVal _ -> return expr
  LVar _ -> return expr
  LAdd x y -> LAdd <$> checkE2 x <*> checkE2 y
  LMinus x y -> LMinus <$> checkE2 x <*> checkE2 y
  LMul x y -> LMul <$> checkE2 x <*> checkE2 y
  LDiv x y -> LDiv <$> checkE2 x <*> checkE2 y

-- basic monad infrastructure

type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

data TcEnv = TcEnv
  { context :: [Var] -- imutable variable list
  }

initTcEnv :: TcEnv
initTcEnv = TcEnv []

insertVar :: Var -> TcM ()
insertVar v = modify (\env -> env{context = v : context env})

removeVar :: Var -> TcM ()
removeVar v = modify (\env -> env{context = (context env) \\ [v]})

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m =
  runIdentity (runStateT (runWriterT (runExceptT m)) env)
