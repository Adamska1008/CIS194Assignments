toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
validate :: Integer -> Bool

toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits x = reverse (toDigitsRev x)

doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : y : zs) = x : 2 * y : doubleEveryOther zs

sumDigits [] = 0
sumDigits (x : ys)
  | x < 10 = x + sumDigits ys
  | otherwise = x `div` 10 + x `mod` 10 + sumDigits ys

validate x = (sumDigits . doubleEveryOther . toDigitsRev) x `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
