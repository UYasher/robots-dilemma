module Lib where

import Data.List (unfoldr)
import Data.String.Utils
import Test.QuickCheck

-- Robots can take two actions: Cooperate (C) or Defect (D)
data Action = C | D deriving (Show)

-- The Robots are playing an iterated prisoner's dilemma.
-- Each iteration of the game is one round of the dilemma.
newtype Iteration = Round (Action, Action) deriving (Show)

-- The history of the game is stored as a stack.
-- The most recent rounds are on the top of the stack.
newtype History = History [Iteration] deriving (Show)

-- A Robot deterministically plays an action in response to a history
-- NOTE: It would be interesting to consider nondeterministic robots,
--       however, that would require a different type signature
newtype Robot = Robot (History -> Action)

-- Enum instances for the above types
-- NOTE: Enum instances are used to allow simpler creation of robots
--       and to generate random values of the above types efficiently

instance Enum Action where
  fromEnum a = case a of
    C -> 0
    D -> 1

  toEnum a = case a of
    0 -> C
    1 -> D

instance Enum Iteration where
  fromEnum (Round i) = case i of
    (C, C) -> 0
    (C, D) -> 1
    (D, C) -> 2
    (D, D) -> 3

  toEnum n = Round $ case n of
    0 -> (C, C)
    1 -> (C, D)
    2 -> (D, C)
    3 -> (D, D)

-- The Enum instance of history works by taking base 4 and shifting it over by one
-- Iterations are base 4 (see Enum instance for Iteration)
-- The empty list is one extra element
-- [] [(C,C)] ... [(C,C), (C,C)] ... [(C,C), (C,C), (C,C)]
-- 0  0 + 1   ...   1*4^1 + 1    ...  1*4^2 + 1*4^1 + 1
instance Enum History where
  fromEnum (History h) = foldl convert 0 h
    where
      convert b x = 4 * b + 1 + fromEnum x

  toEnum n = History . reverse $ unfoldr invert n
    where
      invert b =
        if b == 0
          then Nothing
          else Just (toEnum $ (b - 1) `mod` 4, (b - 1) `quot` 4)

-- NOTE: Can vary the number of rounds taken into consideration using the field notation.
--       Would need to add a field for the number of rounds
makeRobot :: [Action] -> Robot
makeRobot xs
  | length xs == 21 = Robot $ \(History x) -> xs !! fromEnum (History $ take 2 x)
  | otherwise = error "List of actions must have exactly 21 elements"

-- NOTE: Assumes that Robot only looks at last 2 rounds
--       Can be updated with field for number of rounds
instance Show Robot where
  show (Robot f1) = clean . concatMap intToHistoryString $ [0 .. 20]
    where
      intToHistoryString x = let x' = toEnum x :: History in show x' ++ " -> " ++ (show . f1 $ x') ++ "\n"
      clean = replace "History " "" . replace "Round " "" . strip

-- Given two robots and a history, get what the two robots play next
computeRound :: Robot -> Robot -> History -> Iteration
computeRound (Robot f1) (Robot f2) h = Round (f1 h, f2 h)

-- Given two robots and a history, produce a history with n subsequent rounds
computeNRounds :: Int -> Robot -> Robot -> History -> History
computeNRounds 0 _ _ h = h
computeNRounds n r1 r2 h@(History xs) = computeNRounds (n - 1) r1 r2 h'
  where
    h' = History $ computeRound r1 r2 h : xs

-- |   | C     | D
-- |---|-------|-------
-- | C | 1,  1 | -1, 2
-- | D | 2, -1 |  0, 0
-- Score one round for P1 according to the above payoff matrix
scoreRound :: Num a => Iteration -> a
scoreRound (Round i) = case i of
  (C, C) -> 1
  (C, D) -> -1
  (D, C) -> 2
  (D, D) -> 0

-- Swap P1 and P2's payoffs
swap :: Iteration -> Iteration
swap (Round (a, b)) = Round (b, a)

-- Elementwise addition of pairs
(.+) :: Num a => (a, a) -> (a, a) -> (a, a)
(.+) (a, b) (c, d) = (a + c, b + d)

-- Multiplication of a scalar into a pair
(.*) :: Num a => a -> (a, a) -> (a, a)
(.*) i (a, b) = (i * a, i * b)

-- Given a discount factor and a history, compute the scores for both players
scoreHistory :: Num a => a -> History -> (a, a)
scoreHistory delta (History h) = foldr (\x b -> (scoreRound x, scoreRound $ swap x) .+ (delta .* b)) (0, 0) h

-- compute statistics

average :: (Fractional a, Foldable t) => t a -> a
average = (/) <$> sum <*> realToFrac . length

stdev :: [Float] -> Float
stdev xs = sqrt . average . map ((^ 2) . (-) mu) $ xs
  where
    mu = average xs

-- randomly generate robots

-- Generate a random sequence of actions of length 21 and use it to make a robot
randomRobot :: Gen Robot
randomRobot = makeRobot <$> sequence [(\y -> chooseEnum (C, D)) x | x <- [0 .. 20]]

-- Given a robot, compute its performance against 1000 other robots
-- then return the mean performance and the standard deviation
monteCarloStats :: Robot -> Gen [Float]
monteCarloStats r = sequence [average <$> q, stdev <$> q]
  where
    randomBots n = sequence [(\y -> randomRobot) x | x <- [0 .. n]]
    getScores = fmap $ (\x -> scoreHistory 0.8 (computeNRounds 1000 r x (History [])))
    q = (fmap fst) <$> getScores <$> randomBots 1000

-- Use the Monte Carlo method to generate the mean performance
-- and standard deviation of a random robot
randomRobotMCStats :: Gen [Float]
randomRobotMCStats = do
  x <- randomRobot
  monteCarloStats x