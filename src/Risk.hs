{-# LANGUAGE FlexibleInstances #-}

module Risk where

import Data.List
import Data.Monoid

type Army = Int
type Prob = Double
data Battle = Battle { a :: Army, d :: Army , p :: Prob}
  deriving (Show)

instance Ord Battle where
  compare (Battle a1 d1 _) (Battle a2 d2 _)
   | a1 > a2 || (a1 == a2 && d1 > d2) = GT
   | a1 < a2 || (a1 == a2 && d1 < d2) = LT
   | otherwise                        = EQ

instance Eq Battle where
  x == y = (a x) == (a y) && (d x) == (d y)

instance Monoid Battle where
  mempty = Battle 0 0 0
  mappend x y = x {p = (p x + p y)}

type Timestream = [Battle]

-- instance Monoid Timestream where
tempty = []
tappend = foldr bcons
tconcat = foldr tappend tempty

bcons :: Battle -> Timestream -> Timestream
bcons b []    = [b]
bcons b (prev:bs)
  | b == prev = b <> prev : bs
  | b > prev  = b : prev : bs
  | otherwise = prev : bcons b bs

attack :: Battle -> Timestream
attack (Battle a d p)
  | nDice == (3,2) = [Battle  a    (d-2) (2890/7776 * p),
                      Battle (a-1) (d-1) (2611/7776 * p),
                      Battle (a-2)  d    (2275/7776 * p)]
  | nDice == (3,1) = [Battle  a    (d-1) ( 855/1296 * p),
                      Battle (a-1)  d    ( 441/1296 * p)]
  | nDice == (2,2) = [Battle  a    (d-2) ( 295/1296 * p),
                      Battle (a-1) (d-1) ( 420/1296 * p),
                      Battle (a-2)  d    ( 581/1296 * p)]
  | nDice == (2,1) = [Battle  a    (d-1) (  125/216 * p),
                      Battle (a-1)  d    (   91/216 * p)]
  | nDice == (1,2) = [Battle  a    (d-1) (   55/216 * p),
                      Battle (a-1)  d    (  161/216 * p)]
  | nDice == (1,1) = [Battle  a    (d-1) (    15/36 * p),
                      Battle (a-1)  d    (    21/36 * p)]
  | otherwise      = [Battle a d 1]
  where
    nDice = ((min 3 (a - 1)), min 2 d)

invade :: Battle -> Timestream
invade b@(Battle a d p)
  | a < 2 || d == 0 = [b]
  | otherwise = tconcat $ map invade $ attack b

minvade :: Timestream -> Timestream
minvade bs = tconcat [invade b | b <- bs]

campaign :: Army -> [Army] -> Timestream -> [Timestream]
campaign att def [] = campaign att def [Battle (att+1) 0 1]
campaign _ [] _ = []
campaign attackers (defenders:ds) t =
  res : campaign attackers ds res
  where res = tconcat [invade $ Battle ((a b)-1) defenders (p b) | b <- t]

success (Battle _ d _) = (d==0)

successProb :: Army -> Army -> Double
successProb a d = sum [p b | b <- wins]
  where
    wins = filter success $ invade $ Battle a d 1

results :: [Timestream] -> [Double]
results = map (\t -> sum [p b | b <- (filter success t)])

-- main :: IO ()
-- main = do
--   putStr "Attackers: "
--   a <- readLn
--   putStr "Defenders: "
--   d <- readLn
--   putStr "Chance of Success: "
--   putStrLn $ show $ successProb a d

