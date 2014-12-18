{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

-- The themes are A .. Z
-- One run will list pairs (theme, duration) until the movie length is reached.
-- Theme A hash much higher probability as theme Z, succesive repetitions are not accepted
-- Duration is an integer number of frames (between 5 and 100, parameters)
-- Movie length (parameter) is in seconds

framesMin, framesMax :: Int
framesMin = 5
framesMax = 100

movieSecs :: Int
movieSecs = 30 * 60	-- 30 mins

framesPerSec :: Int
framesPerSec = 24

type Distrib = [(Char, Int)]

chooseTheme :: Distrib -> Int -> Char
chooseTheme distr r = c
    where rest = dropWhile ((<r) . snd) distr
          c = fst $ head rest

-- Infinite stream of themes
themes :: RandomGen g => g -> Distrib -> [Char]
themes g distr = map (chooseTheme distr) $ randomRs (1, rmax) g
    where rmax = snd $ last distr

-- Kind of extended duplicate filter
-- Skip elements of an infinite list which have some relation (given as a boolean function)
-- with the previous accepted element
nodups :: (a -> a -> Bool) -> [a] -> [a]
nodups f = go Nothing
    where go Nothing   (a:as) = a : go (Just a) as
          go ja@(Just a') (a:as)
              | f a' a    = go ja as
              | otherwise = a : go (Just a) as

durations :: RandomGen g => g -> [Int]
durations = randomRs (framesMin, framesMax)

movie :: RandomGen g => g -> Int -> [(Char, Int)]
movie g len = map switch $ map snd $ takeWhile ((<flen) . fst) $ drop 1 $ scanl f (0, undefined)
                  $ zip (durations g1) $ nodups (==) (themes g2 revOrdDistr)
    where (g1, g2) = split g
          f (!s, _) (d, t) = (s+d, (d, t))
          flen = len * framesPerSec
          switch (a, b) = (b, a)

-- We try following distributions for the themes:
-- sum [ 1..26 ] = 351, then:
-- A has 26/351, B has 25/351, ..., Z has 1/351

revOrdDistr :: [(Char, Int)]
revOrdDistr = zip ['A'..'Z'] $ tail $ scanl (+) 0 $ reverse [ 1.. 26 ]

main :: IO ()
main = do
    stdg <- getStdGen
    mapM_ putStrLn $ map (\(t, d) -> t : ' ' : ':' : ' ' : show d) $ movie stdg movieSecs
