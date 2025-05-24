class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

-- type class declaration for Applicative
class Applicative f where
    -- injection
    pure  :: a -> f a
    -- apply a function which is itself in a context to a value in a context
    (<*>) :: f (a -> b) -> f a -> f b -- like $ but on computational context f