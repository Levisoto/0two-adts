module LogAnalysis where

import Log

--It is my first Haskell Function -----------------------
-------------------------------------------------------------
parseMessage :: String -> LogMessage
parseMessage msm =
    case filter of
      ("I":ts:xs) -> LogMessage Info (read ts) (unwords xs)
      ("W":ts:xs) -> LogMessage Warning (read ts) (unwords xs)
      ("E":ns:ts:xs) -> LogMessage (Error (read ns)) (read ts) (unwords xs)
      _ -> Unknown msm
    where
      filter = words msm
------------------------------------------
--It is a form to returns a list og LogMessage
------------------------------------------
parse :: String -> [LogMessage]
parse = map parseMessage.lines
------------------------------------------

----THIS is the my second function----#######
insert :: LogMessage -> MessageTree -> MessageTree
insert log Leaf = Node Leaf log Leaf
insert log1@(LogMessage _ time1 _) (Node left log2@(LogMessage _ time2 _) right)
  | time1 > time2 = Node left log2 (insert log1 right)
  | otherwise = Node (insert log1 left) log2 right
insert _ tree = tree
------------------------------------------

build :: [LogMessage] -> MessageTree
build [] = Leaf
build m = foldr insert Leaf m

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left)++[m]++(inOrder right)

extratStr :: [LogMessage] -> [String]
extratStr [] = []
extratStr ((LogMessage (Error ns) _ str):xs)
  | ns >= 50 = str:(extratStr xs)
  | otherwise = extratStr xs 
extratStr (_:xs) = extratStr xs 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong  = extratStr.inOrder.build 
