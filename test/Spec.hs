module Main where 

import Test.Hspec
-- import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Attoparsec.Text
import Data.Text(Text)
import qualified Data.Text as T
import WikiTable


-- Mostly just using this file to test the parsers, since those are most likely 
-- to fail at runtime

main :: IO ()
main = do 
  wikiText <- T.pack <$> readFile "./test/tableText.txt"
  hspec $ do 
    describe "Wiki table" $ do 
      it "parses the header of a wiki table" $ 
        parseOnly parseHeader "!Guest / Host\n!Ordered\n!Rating\n|-"
        `shouldBe`
        Right testHeader
      it "parses a row of a table" $ 
        parseOnly parseTableRow "|-\n|Nick Wiger\n|Sausage, Egg & Cheese Biscuit Taco\nSteak Crunchwrap\n|3 Forks\n"
        `shouldBe`
        Right ["-", "Nick Wiger", "Sausage, Egg & Cheese Biscuit Taco\nSteak Crunchwrap", "3 Forks"]
      it "parses many rows of a table" $ 
        parseOnly parseTableContent "|-\n|Nick Wiger\n|Sausage, Egg & Cheese Biscuit Taco\nSteak Crunchwrap\n|3 Forks\n|-\n|Mike Mitchell\n|Sausage Grande Breakfast Burrito\nBacon Crunchwrap\n\nChicken Biscuit Taco\n|5 Forks\n|-\n|Jack Allison\n|Sausage, Egg & Cheese Biscuit Taco\nBacon Crunchwrap\n|5 Forks\n|}"
        `shouldBe`
        Right testTableContent
      it "parses the wiki table and returns a representation of the table if it is well formed" $ 
        eitherResult (parse parseWikiTable wikiText)
        `shouldBe`
        (Right $ constructTable ["Guest / Host", "Ordered", "Rating"] testTableContent)

testHeader :: [Text]
testHeader = ["Guest / Host", "Ordered", "Rating"]

testTableContent :: [[Text]]
testTableContent = 
  [ 
    [ "Nick Wiger"
    , "Sausage, Egg & Cheese Biscuit Taco\nSteak Crunchwrap"
    , "3 Forks"
    ],
    [ "Mike Mitchell"
    , "Sausage Grande Breakfast Burrito\nBacon Crunchwrap\n\nChicken Biscuit Taco"
    , "5 Forks"
    ],
    [ "Jack Allison"
    , "Sausage, Egg & Cheese Biscuit Taco\nBacon Crunchwrap"
    , "5 Forks"
    ]
  ]
