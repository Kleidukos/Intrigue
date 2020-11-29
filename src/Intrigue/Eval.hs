module Intrigue.Eval where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Intrigue.Types
import Intrigue.Environment

eval :: AST -> EvalM AST
eval ast =
  case ast of
    b@(Bool _)      -> pure b
    s@(String _)    -> pure s
    c@(Character _) -> pure c
    n@(Number _)    -> pure n
    l@Lambda{}      -> pure l
    n@Nil           -> pure n
    Quote expr      -> pure expr
    Atom t          -> evalAtom t
    List v          -> evalList (List v) 

evalList :: AST -> EvalM AST
evalList (List v) =
  case V.head v of
      List listHead -> 
        case V.length v of
          0 -> List <$> traverse eval v
          _ -> do 
            print listHead
            case V.head listHead of
              Atom "lambda" -> do
                let body = listHead V.! 2 
                let parameters = getLambdaParams $ V.head (V.tail listHead) 
                let arguments = V.tail v
                evalLambdaAtom body parameters arguments
              Atom x ->
                applyFun x (V.tail v)
              Lambda parameters body ->
                case V.last v of
                  (List arguments') -> do
                    arguments <- traverse eval arguments'
                    evalLambda parameters body arguments
                  n ->
                    error $ "Error: Bad constructor found when evaluating Lambda, expected List but found: " <> show n
              _ -> do
                pure $ List v -- this is data, return data.
      Lambda parameters body ->
        case V.last v of
          (List arguments') -> do
            arguments <- traverse eval arguments'
            evalLambda parameters body arguments
          n ->
            error $ "Error: Bad constructor found when evaluating Lambda, expected List but found: " <> show n
      x -> do
        let args = V.tail v
        evaluatedArgs <- traverse eval args
        applyFun (getAtomContent x) evaluatedArgs
evalList n =
  error $ "Error: Bad constructor found when evaluating List, expected List but found: " <> show n

evalLambda :: Vector Text -> AST -> Vector AST -> EvalM AST
evalLambda parameters body arguments = do
  Environment{..} <- ask
  let newUserEnv = fromVector (V.zip parameters arguments) <> userEnv
  let newEnv = Environment{userEnv=newUserEnv, primEnv=primEnv}
  local (const newEnv) (eval body)

evalLambdaAtom :: AST -> Vector Text -> Vector AST -> EvalM AST
evalLambdaAtom body parameters arguments =
  eval $ List $ V.fromList [Lambda parameters body, List arguments]

getLambdaParams :: AST -> Vector Text
getLambdaParams (List args) = fmap getAtomContent args
getLambdaParams ast = error $ "Bad datatype for lambda argVector: Expected List, got " <> show ast

fromVector :: (Eq a, Hashable a) => Vector (a, b) -> HashMap a b
fromVector = V.foldr' (\(key, value) acc -> HM.insert key value acc) HM.empty
