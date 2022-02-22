module Generator
    ( Language
    , createLanguage
    , generateOne
    , justOrError
    ) where

import Separator (separate)
import System.Random

type VowelMap = [(Char, [String])]
type ErrorMessage = String

data Language = Lang { startSylls :: [String]
                     , endSylls :: VowelMap
                     , midSylls :: VowelMap
                     , vowels :: String
                     }


createLanguage :: String -> [String] -> Language
createLanguage vowels words = Lang startSylls endSylls midSylls vowels
  where
    separated = filter ((>1) . length) $ map (separate vowels) words
    startSylls = map head separated
    endSylls = getVowelMap (map last separated) vowels
    midSylls = getVowelMap (filter (isMidSyll vowels) $ concat separated) vowels

isMidSyll :: String -> String -> Bool
isMidSyll vowels syll = isVowel (head syll) && isVowel (last syll)
  where
    isVowel :: Char -> Bool
    isVowel = (`elem` vowels)


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


generateOne :: (RandomGen g, Integral a) => Language -> a -> g -> (String, g)
generateOne language syllcount gen1 = (wholeWord, gen4)
  where
    (Lang startSylls endSylls midSylls vowels) = language
    (firstSyll, gen2) = pickRandomFromList startSylls gen1

    (initWord, gen3)
        = (!!(fromIntegral syllcount))
        $ iterate (\(word, genx) -> appendSyll midSylls word genx) (firstSyll, gen2)

    (wholeWord, gen4) = appendSyll endSylls initWord gen3

    appendSyll :: RandomGen g => VowelMap -> String -> g -> (String, g)
    appendSyll vowelMap word gen1 = (word ++ tail nextSyll, gen2)
      where
        possibleSylls = justOrError
                            ("appendSyll: error: No following syllable found for " ++ word)
                            $ lookup (last word) vowelMap
        (nextSyll, gen2) = pickRandomFromList possibleSylls gen1


justOrError :: String -> Maybe a -> a
justOrError msg Nothing = error msg
justOrError _   (Just a)  = a
        

pickRandomFromList :: RandomGen g => [a] -> g -> (a, g)
pickRandomFromList [] _    = error "pickRandomFromList: error: empty list"
pickRandomFromList li gen1 = (li !! ind, gen2)
  where
    (ind, gen2) = uniformR (0, length li - 1) gen1
    
