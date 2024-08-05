{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 = [fib i | i <- [0 ..]]

fibs2 = map fib [0 ..]
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibs2 !! (n - 1) + fibs2 !! (n - 2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a sa) = a : streamToList sa

instance (Show a) => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a sa) = Cons (f a) (streamMap f sa)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats = streamFromSeed (+ 1) 1

ruler = streamMap div2cnt nats
  where
    div2cnt :: Integer -> Integer
    div2cnt n = if n `mod` 2 == 1 then 0 else 1 + div2cnt (n `div` 2)

x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons a as) = Cons (-a) (negate as)
  (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
  (Cons a as) * rb@(Cons b bs) = Cons (a * b) (streamMap (* a) bs + as * rb)

instance Fractional (Stream Integer) where
  (Cons a as) / (Cons b bs) = q
    where
      q = Cons (a `div` b) (streamMap (`div` b) (as - q * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

data Matrix2 = M Integer Integer Integer Integer

instance Num Matrix2 where
  (M a11 a12 a21 a22) + (M b11 b12 b21 b22) = M (a11 + b11) (a12 + b12) (a21 + b21) (a22 + b22)
  (M a11 a12 a21 a22) * (M b11 b12 b21 b22) = M (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22) (a21 * b11 + a22 * b21) (a21 * b21 + a22 * b22)

qpow :: Matrix2 -> Integer -> Matrix2
qpow a 0 = M 1 0 0 1
qpow a n = case even n of
  True -> q * q
    where
      q = qpow a (n `div` 2)
  False -> a * q * q
    where
      q = qpow a ((n - 1) `div` 2)

fibs4 = streamMap get (streamMap (qpow (M 1 1 1 0)) nats)
  where
    get (M a b c d) = b