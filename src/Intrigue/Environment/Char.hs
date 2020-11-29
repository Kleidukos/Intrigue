module Intrigue.Environment.Char where

import qualified Data.Vector as V

import Intrigue.Types

car :: Vector AST -> EvalM AST
car argVec =
  case V.head argVec of
    List args ->
      pure . V.head $ args
    x -> error $ "Type mismatch, expected two characters but got " <> show x

cdr :: Vector AST -> EvalM AST
cdr argVec =
  case V.head argVec of
    List args ->
      pure $ List $ V.tail args
    x -> error $ "Type mismatch, expected two characters but got " <> show x

charEq :: AST -> AST -> EvalM AST
charEq (Character x) (Character y) = pure $ Bool $ x == y
charEq x y = error $ "Type mismatch, expected two characters but got " <> show x <> " and " <> show y

charLT :: AST -> AST -> EvalM AST
charLT (Character x) (Character y) = pure $ Bool $ x < y
charLT x y = error $ "Type mismatch, expected two characters but got " <> show x <> " and " <> show y

charGT :: AST -> AST -> EvalM AST
charGT (Character x) (Character y) = pure $ Bool $ x > y
charGT x y = error $ "Type mismatch, expected two characters but got " <> show x <> " and " <> show y
