module Text.Display where

import Prelude hiding (intercalate)
import qualified Data.Vector  as V

class Display a where
  display :: a -> Text

instance Display (Vector Text) where
  display vector = curly $ V.foldr1' (<>) $ intercalate ", " vector
    where curly x = "{" <> x <> "}"

instance Display Text where
  display = id

unwords :: Vector Text -> Text
unwords vector = V.foldr1' (<>) $ intercalate " " vector

intercalate :: Text -> Vector Text -> Vector Text
intercalate sep vector =
  if V.null vector
  then vector
  else V.tail $ V.concatMap (\word -> V.fromList [sep, word]) vector
