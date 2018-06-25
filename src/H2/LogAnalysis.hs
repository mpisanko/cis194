{-# OPTIONS_GHC -Wall #-}
module H2.LogAnalysis where

import H2.Log

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
  remainder -> (Unknown remainder, "")

parseTime :: Parser Int
parseTime string = case words string of
  (time : xs) -> Just (read time, unwords xs)
  _ -> Nothing

parseLogMessage :: Parser LogMessage
parseLogMessage string = do
    (messageType, remainder) <- parseMessageType string
    (time, remainder') <- parseTime remainder
    pure $ (LogMessage messageType time remainder', "")

someFunc :: IO ()
someFunc = do
  print $ parseLogMessage "E 12 123 abs ederf"
  print $ parseLogMessage "I 123 abs ederf"

parse :: String -> [LogMessage]
parse input = map messageOrNothing $ map parseLogMessage $ lines input
  where
    messageOrNothing (Just (m, "")) = m
    messageOrNothing _ = Nothing


