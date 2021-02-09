module ForkRatings where 

import WikiTable
import Utils
import Control.Lens
import Data.Attoparsec.Text hiding(take, takeWhile)
import qualified Data.Attoparsec.Text as A
import Data.Char
-- import qualified Data.Text as T
import Data.Text (Text)
import Safe
import Control.Monad


data Person
  = Nick 
  | Mitch
  | Guest Text
  deriving(Eq, Show)

data ForkRating = ForkRating
  { person :: Person 
  , rating :: Double
  } deriving(Eq, Show)

data ForkRatings = ForkRatings
  { nick :: ForkRating
  , mitch :: ForkRating
  , guests :: [ForkRating]
  } deriving(Eq, Show)


-- First column is Guest / Host, last column is rating
-- Assumes first row is Nick, second is Mitch, 3rd+ are guest(s), 
-- but if the row has no correspondning rating, the row won't be included,
-- since sometimes there's an extra row
relevantColumns :: WikiTable Text Text -> Either String ForkRatings
relevantColumns table = maybeToEither "Couldn't read fork ratings from table" $ do 
  (peopleKey, ratingsKey) <- liftM2 (,) (headMay header) (lastMay header)
  let people  = column peopleKey table
  let ratings = column ratingsKey table
  nick   <- ForkRating Nick  `liftM` (ratings !? 0 >>= getRating)
  mitch  <- ForkRating Mitch `liftM` (ratings !? 1 >>= getRating)
  guests <- grabGuests people ratings 
  return $ ForkRatings nick mitch guests
  where header   = WikiTable.header table
        arr !? i = arr `atMay` i -- aliasing "atMay" with "!?". Vector does that and I like it.
        grabGuest name rating = ForkRating (Guest name) `liftM` (getRating rating)
        grabGuests people ratings = 
          ratings
          & takeWhile (/= "")
          & zip people
          & drop 2
          <&> uncurry grabGuest
          & foldr (liftM2 (:)) (Just [])

getRating :: Text -> Maybe Double
getRating = maybeResult . parse (A.takeWhile isSpace >> option "" "<s>" >> double)
