module Main where

import Separator (normalizeText, vowelsHu)
import Generator
import System.Random (getStdGen)

main = do
    text <- fmap (map normalizeText . words) $ readFile "testText.txt"
    gen1 <- getStdGen
    let vowels = vowelsHu
        language = createLanguage vowelsHu text
    putStrLn $ fst $ generateOne language 3 gen1

