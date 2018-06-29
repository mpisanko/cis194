{-# OPTIONS_GHC -Wall #-}
module H2.LogAnalysis where

import H2.Log
import Data.Maybe (fromJust)

parseMessage :: String -> LogMessage
parseMessage string = go $ words string
  where
    go ("E":code:time:xs) = LogMessage (Error (read code)) (read time) (unwords xs)
    go ("W":time:xs)      = LogMessage Warning (read time) (unwords xs)
    go ("I":time:xs)      = LogMessage Info (read time) (unwords xs)
    go _                  = Unknown string

type Parser a = String -> Maybe (a, String)
parseMessageType :: Parser MessageType
parseMessageType string = case words string of
  (['E'] : code : remainder) -> Just (Error (read code), unwords remainder)
  (['W'] : remainder) -> Just (Warning, unwords remainder)
  (['I'] : remainder) -> Just (Info, unwords remainder)
  _ -> Nothing

parseTime :: Parser Int
parseTime string = case words string of
  (time : xs) -> Just (read time, unwords xs)
  _ -> Nothing

parseLogMessage :: Parser LogMessage
parseLogMessage string = case validMessage string of
  m@(Just _) -> m
  Nothing -> Just (Unknown string, "")
  where
    validMessage string' = do
      (messageType, remainder) <- parseMessageType string'
      (time, remainder') <- parseTime remainder
      pure $ (LogMessage messageType time remainder', "")

someFunc :: IO ()
someFunc = do
  print $ parseLogMessage "E 12 123 abs ederf"
  print $ parseLogMessage "I 123 abs ederf"
  print $ parseLogMessage "gibberish abs ederf"

parse :: String -> [LogMessage]
parse input = map (fst . fromJust) $ map parseLogMessage $ lines input


