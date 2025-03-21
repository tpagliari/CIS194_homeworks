import Distribution.Simple.Utils (safeHead)
import Prelude hiding (Maybe, Nothing, Just)
-- General construction of a list of elements t
-- t is a type variable which can stand for any type
data List t = Nil | Cons t (List t)

-- General filter function
filterList :: (t -> Bool) -> List t -> List t
filterList _ Nil = Nil
filterList p (Cons x xs)
  | p x = Cons x (filterList p xs)
  | otherwise = filterList p xs

-- General map function
mapList :: (t1 -> t2) -> List t1 -> List t2
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

-- Always use total functions
-- head :: [a] -> a is partial since can't handle empty list
data Maybe t = Nothing | Just t
safeHead :: [t] -> Maybe t
safeHead [] = Nothing
safeHead (x:_) = Just x