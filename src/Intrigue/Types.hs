{-# LANGUAGE StrictData #-}

module Intrigue.Types where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM
import Data.Text
import Data.Text.Display
import qualified Data.Text.Lazy.Builder as B
import Data.Vector
import qualified Data.Vector as V

newtype ASTList = ASTList (Vector AST)
  deriving newtype (Eq, Ord, Show)

data AST
  = Atom {-# UNPACK #-} Text
  | Bool Bool
  | List {-# UNPACK #-} ASTList
  | Number Integer
  | String {-# UNPACK #-} Text
  | Character {-# UNPACK #-} Char
  | Quote AST
  | Lambda
      {-# UNPACK #-} (Vector Text)
      -- ^ Bound names in our body
      AST
      -- ^ Function body
  | Nil
  deriving stock (Show, Eq, Ord)

instance Display AST where
  displayBuilder (Atom atom) = B.fromText atom
  displayBuilder (String str) = "\"" <> B.fromText str <> "\""
  displayBuilder (Character str) = "\\#" <> B.singleton str
  displayBuilder (Quote ast) = "'" <> displayBuilder ast
  displayBuilder (Number num) = displayBuilder num
  displayBuilder (Bool True) = "#t"
  displayBuilder (Bool False) = "#f"
  displayBuilder Nil = "Nil"
  displayBuilder (Lambda _ _) = "<lambda>"
  displayBuilder (List (ASTList contents)) = "(" <> B.fromText (unwordVec (display <$> contents)) <> ")"

data Environment = Environment
  { userEnv :: HashMap Text AST
  , primEnv :: HashMap Text (Vector AST -> EvalM AST)
  }

newtype EvalM a = EvalM {runEval :: ReaderT Environment IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Environment
    , MonadIO
    )

dumpEnv :: EvalM ()
dumpEnv = do
  liftIO $ putStrLn "Env Dump ===="
  Environment{..} <- ask
  let primKeys = HM.keys primEnv
  liftIO $ print primKeys
  liftIO $ print userEnv
  liftIO $ putStrLn "============="

unwordVec :: Vector Text -> Text
unwordVec vector = V.foldr1' (<>) $ intercalateVec " " vector

intercalateVec :: Text -> Vector Text -> Vector Text
intercalateVec sep vector =
  if V.null vector
    then vector
    else V.tail $ V.concatMap (\word -> V.fromList [sep, word]) vector
