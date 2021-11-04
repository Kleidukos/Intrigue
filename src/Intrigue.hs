module Intrigue where

import           Text.Megaparsec      hiding (parse)
import Data.Void
import Data.Text (Text)
import Data.Either
import Control.Monad.Reader

import           Intrigue.Environment
import           Intrigue.Eval
import           Intrigue.Parser
import           Intrigue.Types

parseLine :: Text -> Either (ParseErrorBundle Text Void) AST
parseLine = runParser (parseExp <* eof) "<line>"

evalProgram :: Text -> IO AST
evalProgram program = runReaderT (runEval $ eval ast) baseEnv
  where
    ast = fromRight (error "boo") $ parseLine program

evalM :: EvalM AST -> IO AST
evalM x = runReaderT (runEval x) baseEnv

lambda :: Text
lambda = "((lambda (x y) (+ x y)) 1 2)"
