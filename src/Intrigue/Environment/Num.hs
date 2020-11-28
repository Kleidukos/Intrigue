module Intrigue.Environment.Num where

import Intrigue.Types

add :: Vector AST -> EvalM AST
add operands = pure $ foldl' (\acc number -> applyBinOp (+) number acc ) (Number 0) operands

sub :: Vector AST -> EvalM AST
sub operands = pure $ foldl' (\acc number -> applyBinOp (-) number acc ) (Number 0) operands

numOp :: (Vector AST -> EvalM AST) -> Vector AST -> EvalM AST
numOp fun args = 
  if all isNumber args
  then fun args
  else error $ "Argument mismatch, expected a list of numbers, got " <> show args

isNumber :: AST -> Bool
isNumber (Number _) = True
isNumber _          = False

applyNumber :: (Integer -> Integer) -> AST -> AST
applyNumber f (Number n) = Number $ f n
applyNumber _ x = error $ "Argument mismatch, expected Number, got " <> prettyPrint x

applyBinOp :: (Integer -> Integer -> Integer) -> AST -> AST -> AST
applyBinOp f (Number n) (Number m) = Number $ f n m
applyBinOp _ x y = error $ "Argument mismatch, expected Number, got " <> prettyPrint x <> " and " <> prettyPrint y
