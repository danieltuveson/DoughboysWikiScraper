{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module WikiTable where 

import Data.Maybe
import Data.Char
import Data.Attoparsec.Text hiding(take)
import Control.Applicative ((<|>))
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding(takeWhile)
import Data.Map (Map)
import qualified Data.Map as M

{- 
A Wiki Table is meant to represent the contents of a MediaWiki table. 

Example: 
A | B | C 
---------
1 | 2 | 3 
4 | 5 | 6

Where the content (the numbers) are indexed by column header (the letters) and 
the row number (0 or 1 in this example)

-}


data WikiTable h c = WikiTable
  { header :: [h]
  , wikiMap :: Map (WikiIndex h) c
  } deriving(Show, Eq)

data WikiIndex h = WikiIndex 
  { rowIndex :: Int
  , columnIndex :: h 
  } deriving(Show, Eq, Ord)



-- Takes header and table content, returns WikiTable.
-- Tables should only be constructed using constructTable to ensure 
-- that each row is the same length as the header. 
constructTable :: Ord h => [h] -> [[c]] -> Either String (WikiTable h c)
constructTable header rows
  | foldr ((&&) . areListsSameSize header) True rows = 
    Right $ 
      WikiTable header $ 
        foldr 
          (toWikiTableElement header) 
          M.empty
          (zip [0..] rows)
  | otherwise = Left "Error while constructing table: row sizes are not equal."

-- Helper function for constructTable 
areListsSameSize :: [a] -> [b] -> Bool 
areListsSameSize row1 row2 = length row1 == length row2

-- Helper function for constructTable
-- Takes a header, a row index, and a row list
-- Returns piece of the WikiTable structure containing row / column information for this row
-- and combines it with another partial wiki structure
toWikiTableElement :: Ord h => [h] -> (Int, [c]) -> Map (WikiIndex h) c -> Map (WikiIndex h) c
toWikiTableElement header (i, row) m = 
  M.union m $ 
    foldr (\(h, elt) -> 
      M.insert 
        (WikiIndex i h) elt)
        M.empty 
        (zip header row)

-- Gets element of the WikiTable based on header / row position
cell :: Ord h => WikiIndex h -> WikiTable h c -> Maybe c
cell wi = M.lookup wi . wikiMap

-- Get individual column of a wiki table
-- Returns an empty column if key is invalid
column :: Ord h => h -> WikiTable h c -> [c]
column h = foldr (filterToColumn h) [] . M.toAscList . wikiMap
  where filterToColumn h (key, val) rest
          | columnIndex key == h  = val : rest 
          | otherwise = rest
{-
-- parsers used to generate a wiki table from text
-}

parseWikiTable :: Parser (WikiTable Text Text)
parseWikiTable = do 
  "{| class=\"wikitable article-table\"" <|> "{| class=\"article-table\"" <|> "{| class=\"wikitable\""
  endOfLine
  header <- parseHeader
  rows   <- parseTableContent
  "|}"
  case constructTable header $ rows of 
    Left err    -> fail err 
    Right table -> return table 

-- Grab all headers, init drops the trailing newline
parseHeader :: Parser [Text]
parseHeader = char '!' >> sepBy1 (parseCellContents '!') (char '!')

parseTableRow :: Parser [Text]
parseTableRow = char '|' >> sepBy1 (parseCellContents '|') (char '|')

parseTableContent :: Parser [[Text]]
parseTableContent = "|-" >> endOfLine >> split (== "-") <$> parseTableRow

-- Splits a list into sublists whenever predicate returns true for an element
-- E.g. 
--   genericLines (== "-") ["-", "hi", "-", "hello", "okay", "-", "welp"] 
--   == 
--   [[], ["hi"], ["hello", "okay"], ["welp"]]
split :: (a -> Bool) -> [a] -> [[a]]
split pred ls = 
  let splitup = foldr buildList ([], []) ls in 
    fst splitup : snd splitup
  where buildList elt (sub, rest)
          | pred elt  = ([], sub : rest)
          | otherwise = (elt : sub, rest)

-- Pass in the a character that isn't an end of line character + endChar
-- Arbitrarily picking '?' for begning of the state. All that matters is that it's not an eol or 
parseCellContents :: Char -> Parser Text
parseCellContents initChar = do 
  (t, s) <- runScanner initChar isEndOfCell
  -- if t ended with a newline character, get rid of it
  if isEndOfLine s then 
    return $ T.init t
  else 
    -- This happens when we parse "|}", which denotes the end of the table
    fail "parser has reached end of cell"


-- If we see an end of line character + endChar, we've reached the end of the cell
isEndOfCell :: Char -> Char -> Maybe Char 
isEndOfCell s c
  | endOfCell s c || endOfTable s c = Nothing 
  | otherwise                       = Just c
  where endOfCell s c  = isEndOfLine s && elem c ['!', '|']
        endOfTable s c = elem s ['!', '|'] && c == '}'