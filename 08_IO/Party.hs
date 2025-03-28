module Party where
import Employee

-- A Monoid instance for GuestList
instance Semigroup GuestList where
    (<>) :: GuestList -> GuestList -> GuestList
    (GL lstl fl) <> (GL lstr fr) = GL (lstl ++ lstr) (fl + fr)
instance Monoid GuestList where
    mempty :: GuestList
    mempty = GL [] 0

-- Function to add an Employee to the GuestList without doing any checks
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ fun) (GL lst totFun) = GL (e : lst) (totFun + fun)

-- Returns whichever one of guestlists is more fun
-- Note that we have defined a instance of the Ord class in Employee.hs
moreFun :: GuestList -> GuestList -> GuestList
moreFun x y | x >= y = x | otherwise = y


-- Rose tree
data Tree a = Leaf a | Node a [Tree a]

-- Generic fold function for the rose tree
foldTree :: ([b] -> a -> b) -> b -> Tree a -> b
foldTree f z (Leaf x) = z
foldTree f z (Node x forest) = f (map (foldTree f z) forest) x


-- | The first argument is the “boss” of the current subtree, call him Bob.
-- | The second argument is a list of the results for each subtree under Bob:
-- | nextLevel computes the overall best guest list that includes Bob,
-- and the overall best guest list that doesn’t include Bob
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob gltuples = (funniest withBob, funniest withoutBob)
    where withBob    = map fst gltuples
          withoutBob = map snd gltuples

funniest :: [GuestList] -> GuestList
funniest = foldr moreFun mempty