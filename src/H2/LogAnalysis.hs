{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Maybe (fromJust)
import Data.List (sort)

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
insert m t@(Node left m' right)
  | m == m' = t
  | m < m'  = (Node  (insert m left) m' right)
  | m > m' = (Node left m' (insert m right))

build :: [LogMessage] -> MessageTree
build logs = addLogs logs Leaf
  where
    addLogs [] tree = tree
    addLogs (x : xs) tree = addLogs xs (insert x tree)

parse :: String -> [LogMessage]
parse input = map (fst . fromJust) $ map parseLogMessage $ lines input

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) =
  inOrder left ++ [m] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  (map (\(LogMessage _ _ s) -> s) . sort . filter severe) $ msgs
  where
    severe (LogMessage (Error l) _ _) = l >= 50
    severe _ = False
someFunc :: IO ()
someFunc = do
  print $ parse "E 12 123 abc def ghi\nI 321 qwe rty uio\nW 456 mb vcx z\nnothing at all"
  -- print $ parseLogMessage "E 12 123 abs ederf"
  -- print $ parseLogMessage "I 123 abs ederf"
  -- print $ parseLogMessage "gibberish abs ederf"


