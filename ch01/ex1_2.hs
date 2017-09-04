module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let arg_1 = read $ args !! 0 :: Int 
    let arg_2 = read $ args !! 1 :: Int
    let product = show $ arg_1 * arg_2 :: String 
    putStrLn ("Product is: " ++ product)
