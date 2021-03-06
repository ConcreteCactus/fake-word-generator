module Main where

import Separator (normalizeText, vowelsHu)
import Generator
import System.Random (getStdGen, RandomGen, uniformR)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)

helpText :: String -> String
helpText progName
    =  "Usage: " ++ progName ++ "[flags]\n"
    ++ "\t-h\t\t\tShow this help message\n"
    ++ "\t-c COUNT\t\tSet number of words output (default: 10)\n"
    ++ "\t-r LOWER-UPPER\t\tSet roughly how many syllables to be output (default: 3-4)\n"
    ++ "\t-f TEXTFILE\t\tSet the text file used to generate the words\n"
    ++ "\n\n"

type Options = (Bool, Int, Int, Int, String, String)

defaultOptions = (False, 10, 3, 4, "testText.txt", "aeiou")

parseFlags :: [String] -> Either String Options
parseFlags args = helper args defaultOptions
  where
    helper :: [String] -> Options -> Either String Options
    helper [] opts = Right opts
    helper (x:xs) opts
        | isRecognisedFlag x = case applyOption opts (last x) xs of
                                Right opts -> helper xs opts
                                Left msg   -> Left msg
        | otherwise = helper xs opts

    isRecognisedFlag :: String -> Bool
    isRecognisedFlag str = length str == 2 && head str == '-' && (last str) `elem` "hcrf"

    applyOption :: Options -> Char -> [String] -> Either String Options
    applyOption opts f [] = getOptionHandler f opts ""
    applyOption opts f (x:xs) | isRecognisedFlag x = getOptionHandler f opts ""
    applyOption opts f (x:xs) = getOptionHandler f opts $ filter (not . isSpace) x

    getOptionHandler :: Char -> (Options -> String -> Either String Options)
    getOptionHandler f 
        = justOrError ("Flag -" ++ [f] ++ " has not been recognised. (This error should never evaluate, if you see this, there is probably an internal error.)")
        $ lookup f optionMap

    optionMap :: [(Char, (Options -> String -> Either String Options))]
    optionMap = [ ('h', printHelp)
                , ('c', setCount)
                , ('r', setRange)
                , ('f', setFile)
                ]
    printHelp :: Options -> String -> Either String Options
    printHelp (_, c, rl, ru, f, v) _ = Right (True, c, rl, ru, f, v)

    setCount :: Options -> String -> Either String Options
    setCount (h, _, rl, ru, f, v) arg = case readMaybe arg of
                                        Nothing -> Left (arg ++ " is not a valid number.")
                                        Just c -> Right (h, c, rl, ru, f, v)

    setRange :: Options -> String -> Either String Options
    setRange (h, c, _, _, f, v) arg = case parsed of
                                    Nothing -> Left (arg ++ " is not a valid range.")
                                    Just (upper, lower) -> Right (h, c, lower, upper, f, v)
      where
        parsed = do
            let (upperS, lowerS) = break (=='-') arg
            upper <- readMaybe upperS
            lower <- readMaybe $ tail lowerS
            return (upper, lower)

    setFile :: Options -> String -> Either String Options
    setFile (h, c, rl, ru, _, v) arg = Right (h, c, rl, ru, arg, v)

    setVowels :: Options -> String -> Either String Options
    setVowels (h, c, rl, ru, f, _) arg = Right (h, c, rl, ru, f, arg)

runWithOpts :: Options -> IO ()
runWithOpts (h, c, rl, ru, f, v)
    = if h then do
        progName <- getProgName
        putStrLn (helpText progName)
    else do
        text <- fmap (map normalizeText . words) $ readFile f
        gen1 <- getStdGen
        let vowels = vowelsHu
            language = createLanguage v text
        printWords language rl ru c gen1
        return ()
  where
    printWords :: RandomGen g => Language -> Int -> Int -> Int -> g -> IO g
    printWords _    _  _  0 gen  = return gen
    printWords lang rl ru c gen1 = do
        let (syllCount, gen2) = uniformR (rl, ru) gen1
            (word, gen3)  = generateOne lang syllCount gen2
        putStrLn word
        printWords lang rl ru (c-1) gen3

main = do
    args <- getArgs
    progName <- getProgName

    case parseFlags args of
        Left msg -> hPutStrLn stderr (msg ++ "\n" ++ helpText progName )
        Right opts -> runWithOpts opts
