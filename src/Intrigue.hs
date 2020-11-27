module Intrigue where

import qualified Data.HashMap.Strict as HM
import           Intrigue.Eval
import           Intrigue.Parser
import           Intrigue.Types
import           Text.Megaparsec     hiding (parse)

parseLine :: Text -> Either (ParseErrorBundle Text Void) AST
parseLine = runParser (parseExp <* eof) "<line>"

evalProgram :: Text -> IO AST
evalProgram program = runReaderT (runEval $ eval ast) HM.empty
  where
    ast = fromRight (error "boo") $ parseLine program

lambda :: Text
lambda = "((lambda (x y) (+ x x)) 1 2)"
