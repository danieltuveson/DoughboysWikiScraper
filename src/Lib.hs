{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import WikiTable
import Stats
import ForkRatings
import Utils

import qualified Data.ByteString.Lazy as B
import Data.Aeson.Lens (nth, _String, key)
import Network.Wreq
import Control.Lens
import Text.Pretty.Simple
import Data.Attoparsec.Text hiding(take, takeWhile)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative ((<|>))
import Data.Either
import System.IO
import Network.URI.Encode


scrapeWiki :: IO ()
scrapeWiki = do 
  debug

debug :: IO ()
debug = do 
  episodesOrError <- getListOfEpisodes

  case episodesOrError of 
    Left  error    -> putStrLn error
    Right episodes -> do

      -- Grab the slugs of episodes we will use for querying the API
      let slugs = cleanStub <$> episodes
      
      let isDebug = True

      -- Query the API for ratings
      ratings <-
        if isDebug then
          -- If we're testing, just read from data we pulled earlier
          (read <$> readFile "all_articles.txt") :: IO [Either String Text] 
        
        else do
          -- Grab a list of the articles from the slugs
          r <- mapM (getArticleFromTitleSlug . T.unpack) slugs

          -- Write this to a text file so we can read from it later instead of 
          -- pinging the API a million times
          writeFile "all_articles.txt" $ show r
          return r

      let results = zip slugs $ getRatings <$> ratings
      let bads  = filter (isLeft  . snd) $ results
      let goods = rights $ snd <$> results

      -- Average of each host rating (surprisingly high)
      print $ "Nick Stats: \n"  ++ show (nickStats  goods)
      print $ "Mitch Stats: \n" ++ show (mitchStats goods)

      -- Write files 
      writePretty "bads.txt" bads 
      writePretty "goods.txt" goods 

      -- Some bad values are unavoidable since not all episodes have ratings of a location
      putStrLn $ "Length bads: " ++ show (length bads)
      putStrLn $ "Length goods: " ++ show (length goods)
    where writePretty file = withFile file WriteMode . flip pHPrint

  
  -- Given some article returned by the api, get the ratings table if it exists
getRatings :: Either String Text -> Either String ForkRatings
getRatings articleOrError =
  articleOrError
  >>= parseOnly findRatingsTable
  >>= relevantColumns
  >>= return 

-- Parse until you find the start of the ratings table, then return it 
findRatingsTable :: Parser (WikiTable Text Text)
findRatingsTable = do 
  manyTill anyChar startOfForkRating
  skipWhile (/= '{')
  table <- parseWikiTable
  return table 
  where startOfForkRating = do 
          string "=="
          manyTill anyChar ("rating" <|> "Rating" <|> ("\n" >> fail "not rating table"))

-- Cleans up the link info we grabbed, so it can be turned into a link to another page to scrape
-- E.g.: 
--   Turns: "Rockaroundtheclock doughberfest: [[ampm with Sierra Katow]]"
--   Into:  "ampm with Sierra Katow"
--   Turns: "[[4 - Outback Steakhouse with Jon Gabrus|Outback Steakhouse with Jon Gabrus]]"
--   Into:       "4 - Outback Steakhouse with Jon Gabrus"
cleanStub :: Text -> Text 
cleanStub dirtyStub = 
  if T.take 4 cleanedStub == "http" then 
    handleDumbLink cleanedStub 
  else 
    cleanedStub
  where cleanedStub = 
          dirtyStub 
          -- Turns "some text [[stuff|hi]] more text" into "stuff|hi"
          & T.dropWhile (/= '[')
          & T.dropWhile (== '[')
          & T.dropWhileEnd (/= ']')
          & T.dropWhileEnd (== ']')
          -- "stuff|hi" -> "stuff"
          & T.takeWhileEnd (/= '|') 
          -- Escape encode the stub
          & encodeText

-- Someone put in *1* link starting with http instead of just being a stub, 
-- so we have to handle this stupid case.
-- I'd honestly just update the table, but in the interest of future-proofing, I'm 
-- going to handle it.
handleDumbLink :: Text -> Text 
handleDumbLink stupidLink = 
  stupidLink 
  & T.takeWhileEnd (/= '/')
  & T.takeWhile (/= ' ')

-- Grabs list of episodes it is able to parse from table in the wiki
getListOfEpisodes :: IO (Either String [Text])
getListOfEpisodes = 
  getArticleFromTitleSlug "Doughboys_Episode_List"
  >>= return . getTitleSlugsFromArticle 

-- Gets a list of slugs from the main episode article
-- https://doughboys.fandom.com/wiki/Doughboys_Episode_List
getTitleSlugsFromArticle :: Either String Text -> Either String [Text]
getTitleSlugsFromArticle eitherArticle =
  eitherArticle
  >>= parseOnly getEpisodeList
  >>= return . column "Title"

-- Skip over wiki material we don't care about, then grab the full list
getEpisodeList :: Parser (WikiTable Text Text)
getEpisodeList = do 
  skipWhile (/= '{')
  eitherTable <- parseWikiTable
  return eitherTable

-- Given the slug for a url article in the wiki, return a json representation of the article contents        
getArticleFromTitleSlug :: String -> IO (Either String Text)
getArticleFromTitleSlug articleTitle = do 
  json <- get ("https://doughboys.fandom.com/api.php?action=query&format=json&prop=revisions&titles=" ++ articleTitle ++ "&formatversion=2&rvprop=content&rvslots=*")
  let article = getArticleFromJson json 
  -- Dump the json here so we can debug stuff later
  appendFile "json_dumps.txt" $ show json
  return article

-- This grabs the "content" field from the json returned from the wikimedia api
-- Since the api could change, this could also hit an error
getArticleFromJson :: Response B.ByteString -> Either String Text
getArticleFromJson r = maybeToEither "Failed to parse json content" $ 
  r ^? responseBody
    . key "query"
    . key "pages"
    . nth 0
    . key "revisions"
    . nth 0
    . key "slots"
    . key "main"
    . key "content"
    . _String 