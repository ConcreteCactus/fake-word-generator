module Separator
    ( vowelsHu
    , vowelsEn
    , normalizeText
    , separate
    ) where

import Data.Char (toLower, isLetter)

vowelsHu :: String
vowelsHu = "aáeéiíoóöőuúüű"

vowelsEn :: String
vowelsEn = "aeiou"

isVowel :: String -> Char -> Bool
isVowel vowels = (`elem` vowels)

normalizeText :: String -> String
normalizeText = filter isLetter . map toLower

separate :: String -> String -> [String]
separate vowels word
    | isVowel vowels (head word) = tail $ groupVCV $ groupVC word
    | otherwise                  = groupVCV $ groupVC word
  where
    groupVCV :: [String] -> [String]
    groupVCV [] = []
    groupVCV [x] = [x]
    groupVCV (x:xs)
        | isVowel vowels (head x) = x : (x ++ nextH) : nextT
        | otherwise               = (x ++ nextH) : nextT
      where
        (nextH:nextT) = groupVCV xs

    groupVC :: String -> [String]   
    groupVC []  = []
    groupVC [x] = [[x]]
    groupVC (x:y:xs)
        | isVowel vowels x /= isVowel vowels y = [x] : nextH : nextT
        | otherwise              = (x : nextH) : nextT
      where
        (nextH:nextT) = groupVC (y:xs)


separateHu :: String -> [String]
separateHu = separate vowelsHu

separateEn :: String -> [String]
separateEn = separate vowelsHu
