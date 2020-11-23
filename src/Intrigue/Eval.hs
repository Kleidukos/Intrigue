module Intrigue.Eval where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
-- import Control.Monad.Trans.State.Strict

import Intrigue.Types

eval :: AST -> EvalM AST
eval ast = do
  liftIO $ putTextLn "Evaluating ast"
  case ast of
    b@(Bool _)      -> pure b
    s@(String _)    -> pure s
    c@(Character _) -> pure c
    n@(Number _)    -> pure n
    l@Lambda{}      -> pure l
    n@Nil           -> pure n
    Quote expr      -> pure $ Quote expr
    Atom t          -> evalAtom t
    List v          -> evalList (List v) 

evalAtom :: Text -> EvalM AST
evalAtom t = do
  mVal <- gets (HM.lookup t)
  case mVal of
    Nothing -> error $ "unbound var " <> t
    Just val -> pure val 

evalList :: AST -> EvalM AST
evalList (List v) =
  case V.head v of
      List listHead -> 
        case V.length v of
          0 -> List <$> traverse eval v
          _ -> case V.head listHead of
                Atom "lambda" -> do
                  let body = (V.tail listHead) V.! 1
                  let parameters = getLambdaParams $ (V.head $ V.tail listHead) 
                  let arguments = V.tail v
                  evalLambdaAtom body parameters arguments
                Lambda parameters body ->
                  case V.last v of
                    (List arguments') -> do
                      arguments <- traverse eval arguments'
                      evalLambda parameters body arguments
                    n ->
                      error $ "Error: Bad constructor found when evaluating Lambda, expected List but found: " <> (show n)
                _ -> do
                  pure $ List v -- this is data, return data.
      _ -> pure $ List v
evalList n =
  error $ "Error: Bad constructor found when evaluating List, expected List but found: " <> (show n)

evalLambda :: Vector Text -> AST -> Vector AST -> EvalM AST
evalLambda parameters body arguments = do
  env' <- get
  let env = (fromVector $ V.zip parameters arguments) <> env'
  put env
  eval body

evalLambdaAtom :: AST -> Vector Text -> Vector AST -> EvalM AST
evalLambdaAtom body parameters arguments =
  pure $ List $ V.fromList [Lambda parameters body, List arguments]

getLambdaParams :: AST -> Vector Text
getLambdaParams (List args) = fmap getAtomContent args
getLambdaParams ast = error $ "Bad datatype for lambda argVector: Expected List, got " <> show ast

getAtomContent :: AST -> Text
getAtomContent (Atom content) = content
getAtomContent ast = error $ "Bad datatype: Expected Atom, got " <> prettyPrint ast

-- argsToParams :: Vector Text -> Vector AST -> AST -> AST
-- argsToParams parameters arguments (List body) =
--   fmap (\expr -> 
--           case expr of
--             Atom x ->
--               case HM.lookup x assoc of
--                 Just value -> value
--                 Nothing    -> error $ "Could not find argument for parameter " <> show (Atom x)
--             _ -> expr)
--       body
--   where
--     assoc = fromVector $
--               V.zip parameters arguments
-- argsToParams _ _ _ = error "Bad argument!"

fromVector :: (Eq a, Hashable a) => Vector (a, b) -> HashMap a b
fromVector = V.foldr' (\(key, value) acc -> HM.insert key value acc) HM.empty
