-- Random utility functions 

module Utils where 

maybeToEither :: a -> Maybe b -> Either a b 
maybeToEither errMsg maybe = 
  case maybe of 
    Nothing    -> Left errMsg
    Just stuff -> Right stuff 