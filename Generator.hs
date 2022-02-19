module Generator where

import Separator (separate)
import System.Random

type VowelMap = [(Char, [String])]
type ErrorMessage = String

getVowelMap :: [String] -> String -> VowelMap
getVowelMap words vowels = foldr addToVowelMap emptyMap $ concatMap (separate vowels) words
  where
    emptyMap = map (\v -> (v, [])) vowels

    addToVowelMap :: String -> VowelMap -> VowelMap
    addToVowelMap syll vmap = case new of
        Just rec -> before ++ [rec] ++ after
        Nothing  -> vmap
      where
        before = takeWhile ( (/= (head syll)) . fst ) vmap
        new = (\li -> (head syll, if syll `elem` li then li else syll:li)) <$> lookup (head syll) vmap
        after = tail $ dropWhile ( (/= (head syll)) . fst ) vmap

{-
generator :: RandomGen g -> VowelMap -> g -> String
generator words vowels gen1 = 
-}
