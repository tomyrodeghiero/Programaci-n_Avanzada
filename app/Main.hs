module Main where

main :: IO ()
main = do
    putStrLn "Enter a text:"
    text <- getLine
    putStrLn ("Backwards text: " ++ reverse text)