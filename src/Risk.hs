{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

-- import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

-- newtype DieValue = DV { unDV :: Int }
--   deriving (Eq, Ord, Show, Num)

-- first :: (a -> b) -> (a, c) -> (b, c)
-- first f (a, c) = (f a, c)

-- instance Random DieValue where
--   random           = first DV . randomR (1,6)
--   randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

-- die :: Rand StdGen DieValue
-- die = getRandom

------------------------------------------------------------
-- Risk
-- Ex2 --
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

-- -- Rolls n dice and sorts them in descending order
-- dice ::  Int -> Rand StdGen [DieValue]
-- dice n = do
--   d <- sequence $ replicate n die
--   return $ reverse $ sort $ d

-- -- Returns result of single pair of dice
-- attack :: Battlefield -> (DieValue,DieValue) -> Battlefield
-- attack b (dieA, dieD)
--   | dieA > dieD = b {defenders = defenders b - 1}
--   | otherwise   = b {attackers = attackers b - 1}

-- battle :: Battlefield -> Rand StdGen Battlefield
-- battle b = do
--   diceA <- dice $ min 3 (attackers b - 1)
--   diceD <- dice $ min 2 (defenders b)
--   return $ foldl attack b $ zip diceA diceD

-- -- Ex3 --
-- invade :: Battlefield -> Rand StdGen Battlefield
-- invade b@(Battlefield a d)
--   | a < 2 || d == 0 = return b
--   | otherwise       = battle b >>= invade

-- -- Ex4 --
-- successProb :: Battlefield -> Rand StdGen Double
-- successProb b = do
--   invasions <- sequence $ replicate 1000 $ invade b
--   return $ foldl f 0.0 invasions where
--   f p r = if attackers r > 1 then p + (1/1000) else p

type Prob = Double
data Battle = Battle { a :: Army, d :: Army , p :: Prob}
  deriving (Show)

battleP :: Battle -> [Battle]
battleP (Battle a d p)
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

invadeP :: Battle -> [Battle]
invadeP b@(Battle a d p)
  | a < 2 || d == 0 = [b]
  | otherwise = concat $ map invadeP $ battleP b

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield a d) = sum [p b | b <- wins]
  where
    wins = filter success $ invadeP $ Battle a d 1
    success (Battle a d _) = (d==0)

-- main :: IO ()
-- main = do
--   putStr "Attackers: "
--   a <- readLn
--   putStr "Defenders: "
--   d <- readLn
--   -- res <- evalRandIO $ successProb $ Battlefield a d
--   -- putStr "Simulated: "
--   -- putStrLn (show res)
--   putStr "Exact: "
--   putStrLn $ show $ exactSuccessProb $ Battlefield a d

