{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Text.Read (readMaybe)

parseInt :: String -> Maybe Int
parseInt = readMaybe

parseMessage :: String -> LogMessage
parseMessage msg =
  case words msg of
    ("E" : severity : timestamp : rest) ->
      case (parseInt severity, parseInt timestamp) of
        (Just sev, Just time) -> LogMessage (Error sev) time (unwords rest)
        _ -> Unknown msg
    ("I" : timestamp : rest) ->
      case parseInt timestamp of
        (Just time) -> LogMessage Warning time (unwords rest)
        Nothing -> Unknown msg
    ("W" : timestamp : rest) ->
      case parseInt timestamp of
        (Just time) -> LogMessage Info time (unwords rest)
        Nothing -> Unknown msg
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg@(LogMessage _ ts _) tree = case tree of
  Leaf -> Node Leaf msg Leaf
  Node left rootMsg right -> case rootMsg of
    LogMessage _ rts _ ->
      if ts < rts
        then Node (insert msg left) rootMsg right
        else Node left rootMsg (insert msg right)
    _ -> tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left rootMsg right) = inOrder left ++ [rootMsg] ++ inOrder right

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error ser) _ _) = ser >= 50
isSevereError _ = False

extractMsg :: LogMessage -> String
extractMsg (Unknown msg) = msg
extractMsg (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
-- whatWentWrong logs = map extractMsg (filter isSevereError (inOrder (build logs)))
whatWentWrong logs =
  let severeErrors = filter isSevereError logs
      sortedErrors = inOrder (build severeErrors)
   in map extractMsg sortedErrors