{-# LANGUAGE OverloadedLists #-}
module Intrigue.Environment
  ( module Intrigue.Environment.Num
  , module Intrigue.Environment.Char
  , module Intrigue.Environment.Utils
  , module Intrigue.Environment
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HM

import Intrigue.Types
import Intrigue.Environment.Char
import Intrigue.Environment.Num
import Intrigue.Environment.Utils

baseEnv :: Environment
baseEnv = Environment{primEnv=basePrimEnv, userEnv=HM.empty}

basePrimEnv :: HashMap Text (Vector AST -> EvalM AST)
basePrimEnv = 
  [ ("+", numOp add)
  , ("-", numOp sub)
  , ("number?", isNumber)
  , ("=", numOp equal)
  , ("<", numOp lessThan)
  , (">", numOp moreThan)
  , ("<=", numOp lessOrEqual)
  , (">=", numOp moreOrEqual)
  , ("zero?", numOp isZero)
  , ("positive?", numOp isPositive)
  , ("negative?", numOp isNegative)
  , ("min", numOp minNum)
  , ("max", numOp maxNum)
  , ("car", car)
  , ("cdr", cdr)
  ]
