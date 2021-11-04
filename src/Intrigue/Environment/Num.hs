module Intrigue.Environment.Num where

import Data.Foldable
import Data.Text (unpack)
import Data.Text.Display
import Data.Vector (Vector)
import qualified Data.Vector as V

import Intrigue.Types

add :: Vector AST -> EvalM AST
add operands = pure $ foldl' (\acc number -> applyBinOp (+) number acc ) (Number 0) operands

sub :: Vector AST -> EvalM AST
sub operands = pure $ foldl' (\acc number -> applyBinOp (-) number acc ) (Number 0) operands

isNumber :: Vector AST -> EvalM AST
isNumber args = pure $ Bool $ checkNumber $ V.head args

equal :: Vector AST -> EvalM AST
equal args = pure $ Bool $ all (== hd) tl
  where
    hd = V.head args
    tl = V.tail args

lessThan :: Vector AST -> EvalM AST
lessThan args = pure $ Bool $ transitive (<) args

moreThan :: Vector AST -> EvalM AST
moreThan args = pure $ Bool $ transitive (>) args

lessOrEqual :: Vector AST -> EvalM AST
lessOrEqual args = pure $ Bool $ transitive (<=) args

moreOrEqual :: Vector AST -> EvalM AST
moreOrEqual args = pure $ Bool $ transitive (>=) args

isZero :: Vector AST -> EvalM AST
isZero args =
  case V.head args of
    Number n -> pure $ Bool $ n == 0
    x        -> error $ "Argument mismatch, expected a Number, got " <> show x

isPositive :: Vector AST -> EvalM AST
isPositive args =
  case V.head args of
    Number n -> pure $ Bool $ n > 0
    x        -> error $ "Argument mismatch, expected a Number, got " <> show x

isNegative :: Vector AST -> EvalM AST
isNegative args =
  case V.head args of
    Number n -> pure $ Bool $ n < 0
    x        -> error $ "Argument mismatch, expected a Number, got " <> show x

maxNum :: Vector AST -> EvalM AST
maxNum args = pure $ Number $ V.maximum $ fmap getNumberContent args

minNum :: Vector AST -> EvalM AST
minNum args = pure $ Number $ V.minimum $ fmap getNumberContent args

numOp :: (Vector AST -> EvalM AST) -> Vector AST -> EvalM AST
numOp fun args = 
  if all checkNumber args
  then fun args
  else error $ "Argument mismatch, expected a list of numbers, got " <> show args

transitive :: (AST -> AST -> Bool) -> Vector AST -> Bool
transitive fun args = and $ V.zipWith fun args (V.tail args)

checkNumber :: AST -> Bool
checkNumber (Number _) = True
checkNumber _          = False

getNumberContent :: AST -> Integer
getNumberContent (Number n) = n
getNumberContent x = error $ "Argument mismatch, expected Number, got " <> (unpack $ display x)

applyNumber :: (Integer -> Integer) -> AST -> AST
applyNumber f (Number n) = Number $ f n
applyNumber _ x = error $ "Argument mismatch, expected Number, got " <> (unpack $ display x)

applyBinOp :: (Integer -> Integer -> Integer) -> AST -> AST -> AST
applyBinOp f (Number n) (Number m) = Number $ f n m
applyBinOp _ x y = error $ unpack $ "Argument mismatch, expected Number, got " <> display x <> " and " <> display y
