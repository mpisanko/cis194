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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) t@(Node left m'@(LogMessage _ tsr _) right)
  | ts == tsr = t
  | ts < tsr = (Node  (insert m left) m' right)
  | ts > tsr = (Node left m' (insert m right))

build :: [LogMessage] -> MessageTree
build logs = addLogs logs Leaf
  where
    addLogs [] tree = tree
    addLogs (x : xs) tree = addLogs xs (insert x tree)

someFunc :: IO ()
someFunc = do
  print $ parse "E 12 123 abc def ghi\nI 321 qwe rty uio\nW 456 mb vcx z\nnothing at all"
  -- print $ parseLogMessage "E 12 123 abs ederf"
  -- print $ parseLogMessage "I 123 abs ederf"
  -- print $ parseLogMessage "gibberish abs ederf"

parse :: String -> [LogMessage]
parse input = map (fst . fromJust) $ map parseLogMessage $ lines input


