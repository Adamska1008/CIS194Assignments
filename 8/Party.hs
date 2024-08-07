module Party where

import Data.Tree
import Employee (Employee (Emp), Fun, GuestList (GL), Name, getEmployees, sumFun)

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp n f) gl@(GL es tf) = GL (e : es) (f + tf)

instance Semigroup GuestList where
  (<>) a@(GL al af) b@(GL bl bf) = GL (al ++ bl) (af + bf)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if sumFun a > sumFun b then a else b

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node value subs) = f value (map (treeFold f) subs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss)
  where
    withBoss = mconcat (map snd results)
    withoutBoss = mconcat (map (uncurry moreFun) results)

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun (treeFold nextLevel t)