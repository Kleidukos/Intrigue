module Intrigue.Types where

import Text.Display
import qualified Data.Text as T

data Lisp
  = Atom   Text    
  | Bool   Bool    
  | List   [Lisp]  
  | Number Integer 
  | String Text    
  | Character Char
  | Nil    
  deriving (Show, Eq)

instance Display Lisp where
  display (Atom atom)     = atom
  display (String str)    = "\"" <> str <> "\""
  display (Character str)  = "\\#" <> T.singleton str
  display (Number num)    = T.pack $ show num
  display (Bool True)     = "#t"
  display (Bool False)    = "#f"
  display Nil             = "Nil"
  display (List contents) = "(" <> (T.unwords $ display <$> contents) <> ")"
