module JoinList where

import Sized (Sized, getSize, size)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (mconcat [tag a, tag b]) a b

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i jl@(Append b jlx jly)
  | 0 < i && i < lb = indexJ i jlx
  | lb <= i && i < (getSize . size) b = indexJ (i - lb) jly
  | otherwise = Nothing
  where
    lb = (getSize . size . tag) jlx

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i s@(Single _ _)
  | i == 0 = s
  | otherwise = Empty
dropJ i jl@(Append b jlx jly)
  | i <= 0 = jl
  | 0 < i && i < lb = Append (mconcat [tag djlx, tag jly]) djlx jly
  | lb <= i && i < (getSize . size) b = dropJ (i - lb) jly
  | otherwise = Empty
  where
    lb = (getSize . size . tag) jlx
    djlx = dropJ i jlx

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i s@(Single _ _)
  | i == 1 = s
  | otherwise = Empty
takeJ i jl@(Append b jlx jly)
  | i <= 0 = Empty
  | 0 < i && i < lb = takeJ i jlx
  | lb <= i && i < (getSize . size) b = Append (mconcat [tag jlx, tag njly]) jlx njly
  where
    lb = (getSize . size . tag) jlx
    njly = takeJ (i - lb) jly

  