import Lists
import Prelude hiding ((<*>))

list :: ZipList Int = ZipList [1, 2, 3]
f1, f2, f3 :: Int -> Int
f1 x = x + x
f2 x = x + 2*x
f3 x = x + 3*x
flist :: ZipList (Int -> Int)
flist  = ZipList [f1, f2, f3]
foo :: ZipList Int = flist <*> list