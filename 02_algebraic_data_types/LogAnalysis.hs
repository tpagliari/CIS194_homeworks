{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module LogAnalysis where
import Log
    ( MessageTree(..),
      LogMessage(..),
      TimeStamp,
      MessageType(Error, Info, Warning) )

-- Parse an individual log line from the file
parseMessage :: String -> LogMessage
parseMessage str = case words str of
  ("I":ts:lst) -> LogMessage Info (read ts) (unwords lst)
  ("W":ts:lst) -> LogMessage Warning (read ts) (unwords lst)
  ("E":code:ts:lst) -> LogMessage (Error (read code)) (read ts) (unwords lst)
  _ -> Unknown str

-- Parse a whole log file given as a String
parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)


-- Organize log messages in a recoursive adt: a message tree

-- Helper: get timestamp
getTimestamp :: LogMessage -> TimeStamp
getTimestamp (LogMessage _ ts _) = ts
getTimestamp (Unknown _) = 0

-- Inserts a new LogMessage into an existing MessageTree,
-- producing a new MessageTree.
-- Assume that it is given a sorted MessageTree, and must produce 
-- a new sorted MessageTree containing the new LogMessage as addition
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node ltree oldMsg rtree) = case getTimestamp msg of
  ts | ts < getTimestamp oldMsg -> Node (insert msg ltree) oldMsg rtree
     | otherwise -> Node ltree oldMsg (insert msg rtree)

-- Build a complete MessageTree from a list of messages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:lst) = insert msg (build lst)

-- in-order traversal of the sorted tree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree msg rtree) = inOrder ltree ++ [msg] ++ inOrder rtree


-- Extract errors with a severity of at least 50

-- Strips extra information from LogMessages, leaves only list of Strings
--extractMsg :: [LogMessage] -> [String]
--extractMsg ((LogMessage _ _ msg):lst) = msg : extractMsg lst
--extractMsg _ = []
extractMsg :: [LogMessage] -> [String]
extractMsg [] = []
extractMsg ((LogMessage _ _ msg):lst) = msg : extractMsg lst
extractMsg ((Unknown msg):lst) = msg : extractMsg lst


-- A predicate that is True when the error code is above a min lvl of severity
severe :: Int -> LogMessage -> Bool
severe lvlMin (LogMessage (Error lvl) _ _)
  | lvl > lvlMin = True
  | otherwise = False
severe _ _ = False

-- Takes unsorted list of LogMessages and returns a list of the messages
-- corresponding to any errors with a severity of 50 or greater, sorted by
-- timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lst = extractMsg (filter (severe 50) (inOrder (build lst)))