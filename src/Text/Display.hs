module Text.Display where

class Display a where
  display :: a -> Text
