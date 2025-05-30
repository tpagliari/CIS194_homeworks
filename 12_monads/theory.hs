-- Maybe
data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap _ Nothing'  = Nothing'
    fmap g (Just' x) = Just' . g $ x 

instance Applicative Maybe' where
    pure :: a -> Maybe' a
    pure = Just'

    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    Nothing' <*> _ = Nothing'
    _ <*> Nothing' = Nothing'
    Just' g <*> Just' x = pure . g $ x

instance Monad Maybe' where
    return :: a -> Maybe' a
    return = pure

    (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
    Nothing' >>= g = Nothing'
    Just' x >>= g  = g x

    (>>) :: Maybe' a -> Maybe' b -> Maybe' b
    ma >> mb = ma >>= const mb

