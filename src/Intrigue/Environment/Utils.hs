{-# LANGUAGE OverloadedLists #-}

module Intrigue.Environment.Utils where

import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, unpack)
import Data.Text.Display
import Data.Vector (Vector)

import Intrigue.Types

applyFun :: Text -> Vector AST -> EvalM AST
applyFun atom args = do
  Environment{..} <- ask
  case HM.lookup atom userEnv of
    Just fun -> pure $ List $ ASTList $ [fun] <> args
    Nothing ->
      case HM.lookup atom primEnv of
        Just fun -> fun args
        Nothing -> error $ "Unknown function " <> unpack atom

evalAtom :: Text -> EvalM AST
evalAtom t = do
  Environment{..} <- ask
  case HM.lookup t userEnv of
    Just val -> pure val
    Nothing -> error $ "Unbound atom " <> unpack t

lookupPrim :: Text -> EvalM (Vector AST -> EvalM AST)
lookupPrim t = do
  Environment{primEnv} <- ask
  case HM.lookup t primEnv of
    Just fun -> pure fun
    Nothing -> error $ "Unknown function " <> unpack t

getAtomContent :: AST -> Text
getAtomContent (Atom content) = content
getAtomContent ast = error $ "Bad datatype: Expected Atom, got " <> (unpack $ display ast)
