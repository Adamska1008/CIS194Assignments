import Data.Set (fromList)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate gen
  where
    gen n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

data Tree t
  = Leaf
  | Node Integer (Tree t) t (Tree t)
  deriving (Show, Eq)

foldTree :: [t] -> Tree t
foldTree = foldr insert Leaf
  where
    insert :: t -> Tree t -> Tree t
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node h left val right)
      | height left <= height right =
          let newLeft = insert x left
              newHeight = 1 + max (height newLeft) (height right)
           in Node newHeight newLeft val right
      | otherwise =
          let newRight = insert x right
              newHeight = 1 + max (height newRight) (height left)
           in Node newHeight left val newRight
      where
        height :: Tree t -> Integer
        height Leaf = -1
        height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldr (\a b -> (a && not b) || (not a && b)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let surely_no = fromList [i + j + 2 * i * j | i <- [1 .. n], j <- [i .. n]]
   in filter odd [x | x <- [1 .. 2 * n + 2], x `notElem` surely_no]