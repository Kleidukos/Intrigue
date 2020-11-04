module Test.Parsing where

-- import qualified Test.Tasty
import Test.Tasty.Hspec
import qualified Data.Vector as V
import Text.Megaparsec hiding (parse)

import Intrigue.Types
import Intrigue.Parser

spec :: Spec
spec = parallel $ do
  describe "Atoms" $ do
    let parse = runParser (parseAtom <* eof) "<test>"
    it "lambda" $ do
       parse "lambda" `shouldBe` Right ( Atom "lambda" )
    it "q" $ do
      parse "q" `shouldBe` Right ( Atom "q" )
    it "list->vector" $ do
      parse "list->vector" `shouldBe` Right ( Atom "list->vector" )
    it "+" $ do
      parse "+" `shouldBe` Right ( Atom "+" )
    it "V17a" $ do
      parse "V17a" `shouldBe` Right ( Atom "V17a" )
    it "<=?" $ do
      parse "<=?" `shouldBe` Right ( Atom "<=?" )
  describe "Strings" $ do
    let parse = runParser (parseText <* eof) "<test>"
    it "escape \\\" from tarkov" $ do
      parse "\"escape \\\" from tarkov\""
        `shouldBe` Right ( String "escape \" from tarkov" )
  describe "Numbers" $ do
    let parseUnsigned = runParser (parseNumber <* eof) "<test>"
    let parseSigned   = runParser (parseNegNumber <* eof) "<test>"
    it "Unsigned number" $ do
      parseUnsigned "3" `shouldBe` Right ( Number 3 )
    it "Signed number" $ do
      parseSigned "-3" `shouldBe` Right ( Number (-3) )
  describe "Quoted expression" $ do
    let parse = runParser (parseQuote <* eof) "<test>"
    it "'(1 2 3)" $ do
      parse "'(1 2 3)"
        `shouldBe` Right ( Quote (List $ V.fromList [ Number 1, Number 2, Number 3 ] ))
  describe "S-Expression" $ do
    let parse = runParser (parseSExp <* eof) "<test>"
    it "(eqv? 'a 'a)" $ do
      parse "(eqv? 'a 'a)"
        `shouldBe` Right ( List $ V.fromList
                             [Atom "eqv?",Quote (Atom "a"),Quote (Atom "a")]
                         )
  describe "Nil" $
    it "Nil" $ runParser (parseNil <* eof) "<test>" "Nil"
      `shouldBe` Right ( Nil )
  describe "Character" $ do
    let parse = runParser (parseCharacter <* eof) "<test>"
    it "#\\a" $
      parse "#\\a"
        `shouldBe` Right ( Character 'a')
    it "#\\)" $
      parse "#\\)"
        `shouldBe` Right ( Character ')')
    it "#\\space" $
      parse "#\\space"
        `shouldBe` Right ( Character ' ' )
    it "#\\newline" $
      parse "#\\newline"
        `shouldBe` Right ( Character '\n' )
  describe "Full expression" $ do
    let parse = runParser (parseExp <* eof) "<test>"
    it "factorial" $
      parse "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"
        `shouldBe` Right (List $ V.fromList [Atom "define", List $ V.fromList [Atom "factorial",Atom "n"],List $ V.fromList [Atom "if",List $ V.fromList [Atom "=",Atom "n",Number 0],Number 1,List $ V.fromList [Atom "*",Atom "n",List $ V.fromList [Atom "factorial",List $ V.fromList [Atom "-",Atom "n",Number 1]]]]])
