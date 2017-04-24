{-# LANGUAGE CPP #-}

module Plugins.TIL(TIL(..)) where

import Plugins
import Network.HTTP.Conduit
import Data.List.Split
import System.Process

--url = "http://api.forismatic.com/api/1.0/?method=getQuote&format=json&lang=en"
url = "https://bewakes.udghos.com/til/api/?random=1"

{-
parseSimpleJson :: String -> [(String, String)]
parseSimpleJson "" = []
parseSimpleJson raw = parse 0 $ tail raw -- omit first curly
    where parse i "" = []
          parse i str = keyVal
            where keyVal = map getKeyVal commaSplitted
                  commaSplitted = splitOn "\", \"" str
                  getKeyVal s = (splitted !! 1, splitted !! 2)
                    where splitted = splitOn "\":\"" $ tail s

getVal :: [(String, String)] -> String -> String
getVal lst key = ( snd . head) $ filter (\(x,y)-> x==key) lst

getQuote :: IO String
getQuote = do
    resp <- simpleHttp (getRequest url)
    js <- fmap  parseSimpleJson $ responseBody resp
    return (getVal js "quoteText")
-}

-- ONLY THIS IS USED FOR NOW
getCommandOutput :: String -> [String] -> String -> IO String
getCommandOutput cmd args stin = do
    (ec, o, er) <- readProcessWithExitCode cmd args stin
    return o

data TIL = TIL String String Int
    deriving (Read, Show)

instance Exec TIL where
    alias (TIL _ a _) = a
    run (TIL d _ _) = getCommandOutput "til" [] ""
    rate (TIL _ _ r) = r
