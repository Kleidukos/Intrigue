{-# LANGUAGE OverloadedLists #-}
module Intrigue.Environment.Utils where

import qualified Data.HashMap.Strict as HM
-- import qualified Data.Vector         as V

import Intrigue.Types

applyFun :: Text -> Vector AST -> EvalM AST
applyFun atom args = do
  Environment{..} <- ask
  case HM.lookup atom userEnv of
    Just fun -> pure $ List $ [fun] <> args
    Nothing  ->
      case HM.lookup atom primEnv of
        Just fun -> fun args
        Nothing  -> error $ "Unknown function " <> atom

evalAtom :: Text -> EvalM AST
evalAtom t = do
  Environment{..} <- ask
  case HM.lookup t userEnv of
    Just val -> pure val 
    Nothing  -> error $ "Unbound atom " <> t

lookupPrim :: Text -> EvalM (Vector AST -> EvalM AST)
lookupPrim t = do
  Environment{primEnv} <- ask
  case HM.lookup t primEnv of
    Just fun -> pure fun
    Nothing  -> error $ "Unknown function " <> t

getAtomContent :: AST -> Text
getAtomContent (Atom content) = content
getAtomContent ast = error $ "Bad datatype: Expected Atom, got " <> prettyPrint ast

-- newtype UnaryFunction = UF { runUF :: AST -> EvalM AST }
-- newtype BinaryFunction = BF { runBF :: AST -> AST -> EvalM AST }

-- binaryFold :: BinaryFunction -> AST -> Vector AST -> EvalM AST
-- binaryFold function startValue arguments = foldrM (runBF function) startValue arguments

-- numOp :: (Integer -> Integer -> Integer) -> AST -> AST -> EvalM AST
-- numOp fun (Number x) (Number y) = pure $ Number $ fun x y 
-- numOp _ Nil y = pure y
-- numOp _ x Nil = pure x
-- numOp _ x y   = error $ "Type mismatch, got first argument as " <> prettyPrint x <> " and second argument as " <> prettyPrint y

-- unaryWrapper :: UnaryFunction -> Vector AST -> EvalM AST
-- unaryWrapper fun argVector = runUF fun argument
--   where argument = V.head argVector

-- binaryWrapper :: BinaryFunction -> Vector AST -> EvalM AST
-- binaryWrapper fun argVector = runBF fun argument1 argument2
--   where
--     argument1 = argVector V.! 1
--     argument2 = argVector V.! 2
