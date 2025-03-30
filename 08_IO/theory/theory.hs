module Main where

main :: IO ()
main = putStrLn "Hello from haskell!"
    >> putStrLn "Please, enter a number: "
    >> (readLn >>= (print . succ . fromIntegral))