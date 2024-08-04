module Golf where

import Text.Read (Lexeme (String))

collectEvery :: [t] -> Int -> [t]
collectEvery xs e = case drop (e - 1) xs of
  [] -> []
  x : xs -> x : collectEvery xs e

skips :: [t] -> [[t]]
skips xs = [collectEvery xs n | n <- [1 .. length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [xs !! i | i <- [1 .. length xs - 2], xs !! i > xs !! (i - 1), xs !! i > xs !! (i + 1)]

count :: [Integer] -> [Integer]
count = foldl increment (replicate 10 0)
  where
    increment :: [Integer] -> Integer -> [Integer]
    increment counts x = prefix ++ [head rest + 1] ++ tail rest
      where
        (prefix, rest) = splitAt (fromInteger x) counts

histogram :: [Integer] -> String
histogram xs = unlines (reverse lines ++ ["==========", "0123456789"])
  where
    counts = count xs
    maxCount = maximum counts
    lines = [[if count >= i then '*' else ' ' | count <- counts] | i <- [1 .. maxCount]]