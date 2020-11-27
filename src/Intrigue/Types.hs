{-# LANGUAGE StrictData #-}
module Intrigue.Types where

import qualified Data.Text    as T
import qualified Text.Display as D

data AST
  = Atom      {-# UNPACK #-} Text
  | Bool                     Bool
  | List      {-# UNPACK #-} (Vector AST)
  | Number    {-# UNPACK #-} Integer
  | String    {-# UNPACK #-} Text
  | Character {-# UNPACK #-} Char
  | Quote     {-# UNPACK #-} AST
  | Lambda    {-# UNPACK #-} (Vector Text) -- ^ Bound names in our body
                             AST           -- ^ Function body
  | Nil
  deriving stock (Show, Eq)


prettyPrint :: AST -> Text
prettyPrint (Atom atom)        = atom
prettyPrint (String str)       = "\"" <> str <> "\""
prettyPrint (Character str)    = "\\#" <> T.singleton str
prettyPrint (Quote ast)        = "'" <> prettyPrint ast
prettyPrint (Number num)       = T.pack $ show num
prettyPrint (Bool True)        = "#t"
prettyPrint (Bool False)       = "#f"
prettyPrint Nil                = "Nil"
prettyPrint (Lambda _ _ )      = "<lambda>"
prettyPrint (List contents)    = "(" <> D.unwords (prettyPrint <$> contents) <> ")"

newtype EvalM (a :: Type) = EvalM {runEval :: ReaderT (HashMap Text AST) IO a}
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadReader (HashMap Text AST) 
                   , MonadIO
                   )
