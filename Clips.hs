module Main where

import Control.Applicative ((<$>))
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Random

data Clip = Clip FilePath Double

data Theme = Theme FilePath [Clip]

data Movie = Movie (Maybe FilePath) [Theme]	-- last chosen theme, if any

-- Remove one existing clip from a movie
-- and remember the last chosen theme
-- an empty theme will be removed too
rmClip :: FilePath -> FilePath -> Movie -> Movie
rmClip theme clip (Movie _ ts)
    | null cs   = Movie (Just theme) $ ts1 ++ ts2
    | otherwise = Movie (Just theme) $ ts1 ++ (Theme fp cs) : ts2
    where (ts1, Theme fp (Clip clip _ : cs) : ts2) = break (\(Theme fp _) -> fp == theme) ts

-- Sum the weights of a list of themes
sumThemes :: [Theme] -> Double
sumThemes = sum . concatMap (\(Theme _ cs) -> map (\(Clip _ x) -> x) cs)

-- The next clip can't be from the previous theme
nextThemes :: Movie -> [Theme]
nextThemes (Movie mt ts) = ts'
    where ts' | Just lp <- mt = filter (\(Theme tp _) -> tp /= lp) ts
              | otherwise     = ts

-- Given a (random) double between 0 and 1, select
-- the corresponding clip from the normalised movie
pickClip :: Double -> Movie -> Maybe (FilePath, FilePath)
pickClip r mv = go (r*s) cts
    where go _ []                     = Nothing
          go _ [Theme tp [Clip cp _]] = Just (tp, cp)		-- last theme, last clip
          go r (Theme tp ((Clip cp p : cs)) : ts)
              | r < 0     = Just (tp, cp)			-- found
              | null cs   = go (r-p) ts				-- next theme
              | otherwise = go (r-p) ((Theme tp cs) : ts)	-- next clip
          cts = nextThemes mv
          s   = sumThemes cts

genSeq :: RandomGen g => (g, Movie) -> [(FilePath, FilePath)]
genSeq = unfoldr f
    where f (g, mv) = let (r, g') = random g
                      in case pickClip r mv of
                             Nothing -> Nothing
                             Just pp -> Just (pp, (g', uncurry rmClip pp mv))

setProb :: Double -> Clip -> Clip
setProb p (Clip cp _) = Clip cp p

perClip :: [Double] -> Movie -> Movie
perClip dbls (Movie mf ts) = Movie mf $ map f $ zip dbls ts
    where f (d, Theme tp cs) = Theme tp $ map (setProb d) cs

-- Exponential distribution
expo :: [Double]
expo = 1 : map (* 0.5) expo

ignoreDot :: [FilePath] -> [FilePath]
ignoreDot = filter (not . isPrefixOf ".")

main :: IO ()
main = do
    cdir  <- getCurrentDirectory
    putStrLn $ "Reading themes from " ++ cdir
    tdirs <- sort . ignoreDot <$> getDirectoryContents cdir
    ts    <- forM tdirs $ \d -> do
                 let tp = cdir </> d
                 isdir <- doesDirectoryExist tp
                 if isdir
                    then do
                        putStrLn $ "Reading clips from " ++ tp
                        cs <- map (\c -> Clip c 1) . ignoreDot <$> getDirectoryContents tp
                        return $ Just $ Theme d cs
                    else return Nothing
    let mv' = Movie Nothing $ catMaybes ts
        mv  = perClip expo mv'
    stdg  <- getStdGen
    let pps = genSeq (stdg, mv)
    writeFile "Clip Liste.txt" $ unlines $ map (\(tp, cp) -> "Clip: " ++ tp ++ " : " ++ cp) pps
