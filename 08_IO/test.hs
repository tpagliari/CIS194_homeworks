module Main where
import Party
import Employee

main :: IO ()
main = readFile "company.txt" >>= putStrLn . computeOutput

computeOutput :: String -> String
computeOutput = formatGL . maxFun . read

formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
  where employees = map (\(Emp {empName = name}) -> name) lst