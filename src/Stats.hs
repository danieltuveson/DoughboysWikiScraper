module Stats where 

import ForkRatings

data Stats = Stats 
  { sMean :: Double 
  , sMin :: Double
  , sMax :: Double
  };

instance Show Stats where 
  show stats = 
    "Mean: " ++ show (sMean stats) ++ "\n" ++
    "Min: "  ++ show (sMin stats)  ++ "\n" ++
    "Max: "  ++ show (sMax stats)  ++ "\n"

type Reviewer = ForkRatings -> ForkRating

type Statistic = [Double] -> Double


mean :: Statistic
mean ls = sum ls / fromIntegral (length ls)

reviewersRating :: Reviewer -> ForkRatings -> Double
reviewersRating person = rating . person

getStatistic :: Statistic -> Reviewer -> [ForkRatings] -> Double
getStatistic stat reviewer ratings = stat $ (reviewersRating reviewer) <$> ratings

-- Given a reviewer  (Nick or Mitch) extract the relevant stat
reviewerMean :: Reviewer -> [ForkRatings] -> Double
reviewerMean = getStatistic mean

reviewerMin :: Reviewer -> [ForkRatings] -> Double
reviewerMin = getStatistic minimum

reviewerMax :: Reviewer -> [ForkRatings] -> Double
reviewerMax = getStatistic maximum

reviewerStats :: Reviewer -> [ForkRatings] -> Stats
reviewerStats rev ratings = undefined
  Stats 
    (reviewerMean rev ratings)
    (reviewerMin  rev ratings)
    (reviewerMax  rev ratings)

nickStats :: [ForkRatings] -> Stats
nickStats = reviewerStats nick 

mitchStats :: [ForkRatings] -> Stats
mitchStats = reviewerStats mitch 