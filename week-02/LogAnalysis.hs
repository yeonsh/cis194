{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

parseMessage :: String -> LogMessage
parseMessage s
    | len < 3 = Unknown s
    | a == "I" = LogMessage Info (read n1 :: Int) (unwords (n2:str))
    | a == "E" = LogMessage (Error (read n1 :: Int)) (read n2 :: Int) (unwords str)
    | otherwise = Unknown (unwords (a:n1:n2:str))
    where
        len = length $ words s
        (a:n1:n2:str) = words s

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node left lm1@(LogMessage _ ts0 _) right)
    | ts <= ts0 = Node (insert lm left) lm1 right
    | ts > ts0 = Node left lm1 (insert lm right)
insert (LogMessage _ _ _ ) mt@(Node _ (Unknown _) _) = mt

build :: [LogMessage] -> MessageTree
build xs = foldl (\acc x -> insert x acc) Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++  (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = extractStr $ filterSeverity 50 $ filterErrors $ inOrder $ build xs

filterErrors :: [LogMessage] -> [LogMessage]
filterErrors x = filter isError x
    where
        isError (LogMessage (Error _) _ _) = True
        isError _ = False

filterSeverity :: Int -> [LogMessage] -> [LogMessage]
filterSeverity severityLevel xs = [lm | lm@(LogMessage (Error ec) _ _) <- xs, ec > severityLevel]

extractStr :: [LogMessage] -> [String]
extractStr xs = [str | LogMessage _ _ str <- xs]


-- Sample data

l1 :: LogMessage
l1 = parseMessage "I 29 la la la"

l2 :: LogMessage
l2 = parseMessage "I 39 la la la"

l3 :: LogMessage
l3 = parseMessage "I 19 la la la"

l4 :: LogMessage
l4 = parseMessage "I 9 la la la"

l5 :: LogMessage
l5 = parseMessage "I 119 la la la"

l6 :: LogMessage
l6 = parseMessage "E 20 2 Too many pickles"

l7 :: LogMessage
l7 = parseMessage "E 70 3 Way too many pickles"

l8 :: LogMessage
l8 = parseMessage "E 65 8 Bad pickle-flange interaction detected"

lms :: [LogMessage]
lms = [l1, l2, l3, l4, l5, l6, l7, l8]

mt1 :: MessageTree
mt1 = build lms
