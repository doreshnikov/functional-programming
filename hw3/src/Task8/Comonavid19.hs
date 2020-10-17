{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Task8.Comonavid19
-- Description : 'Grid'-based epidemic simulation
--
-- Model and simulation functions for epidemic infections based on
-- 'Grid' 'Comonad'.
module Task8.Comonavid19
  ( -- * Types
    Config (..),
    Model,
    Person (..),
    Status (..),

    -- * Simulation functions
    patientZeroCase,
    printGrid,
    simulateOne,
    simulateMany,
    stepOne,
    stepMany,
    toChar
  )
where

import Control.Comonad
import System.Console.ANSI
import System.Random

import Task8.Grid

-- | Data type representing a patient's status being infected or healthy.
data Status
    -- | A constructor of 'Status' representing infection with no symptoms
  = Infected Int
    -- | A constructor of 'Status' representing full infection
  | Symptomatic Int
    -- | A constructor of 'Status' representing either healthy or immune person
  | Immune Int
  deriving (Eq, Show)

-- | Data type representing a person (having his own random generator).
data Person = Person
  { -- A person's 'Status' of his health
    status :: Status
    -- A person's random number generator for infection check
  , rnd :: StdGen
  } deriving (Show)

-- | Data type representing a run configuration.
data Config = Config
  { -- An initial simulation random seed
    seed :: Int
    -- A size of a grid in output
  , size :: Int
    -- A probability of infecting another person
  , p :: Double
    -- Incubation period length in days
  , incubation :: Int
    -- Symptomatic period length in days
  , symptoms :: Int
    -- Immunity period length in days
  , immunity :: Int
  } deriving (Eq, Show, Read)

-- | Type representing our simulations' model.
type Model = Grid Person

-- | Creates a 'Char' representation of a given status.
toChar :: Status -> Char
toChar (Infected _) = '.'
toChar (Symptomatic _) = '#'
toChar (Immune 0) = ' '
toChar _ = '@'

-- | Creates a 'Person' infected in given moment according to given 'Config'.
justInfected :: Config -> Person
justInfected config = Person (Infected $ incubation config) (mkStdGen 0)

-- | Creates an initial 'Model' according to given 'Config'.
patientZeroCase :: Config -> Model
patientZeroCase config = replace healthy (justInfected config)
  where
    healthy :: Model
    healthy = Person (Immune 0) . mkStdGen <$> Grid (genericMove (fmap (* 2)) (fmap (* 3)) t)
      where
        t :: ListZipper Int
        t = LZ (randoms $ mkStdGen irnd) 177013 (randoms $ mkStdGen (- irnd))
        irnd :: Int
        irnd = seed config

-- | Checks whether given 'Person' is infected or not.
infected :: Person -> Bool
infected (Person (Immune _) _) = False
infected _ = True

-- | Counts a number of infected neighbours.
infectedNeighbours :: Model -> Int
infectedNeighbours g = length $ filter infected $
  map (\dir -> extract $ dir g) neighbours

-- | Tries to infect a person with no immunity using his 'StdGen'.
-- If immunity is over, sets his immunity status to zero.
infect :: Config -> Person -> Int -> Person
infect config person cnt
  | any ((< p config) . fst) items = (justInfected config) {rnd = snd $ last items}
  | otherwise = Person (Immune 0) (snd $ last items)
  where
    items :: [(Double, StdGen)]
    items = take (cnt + 1) $ iterate (random . snd) (1.0, rnd person)

-- | Updates a 'Person'\'s 'Status' basing on his status and status of
-- his neighbours.
update :: Config -> Model -> Person
update config model =
  let patient = extract model
   in case status patient of
        (Infected x)
          | x > 1 -> patient {status = Infected $ x - 1}
          | otherwise -> patient {status = Symptomatic $ symptoms config}
        (Symptomatic x)
          | x > 1 -> patient {status = Symptomatic $ x - 1}
          | otherwise -> patient {status = Immune $ immunity config}
        (Immune x)
          | x > 1 -> patient {status = Immune $ x - 1}
          | otherwise -> infect config patient (infectedNeighbours model)

-- | Creates an 'IO' action to print our 'Model' with given 'Config'.
printGrid :: Config -> Model -> IO ()
printGrid config model = do
  let grid = cutGrid (size config) $ toChar . status <$> model
  mapM_ putStrLn grid

-- | Performs one 'Model' simulation step.
stepOne :: Config -> Model -> Model
stepOne config = extend (update config)

-- | Performs a given number of 'Model' simulation steps.
stepMany :: Config -> Int -> Model -> Model
stepMany config days model
  | days == 0 = model
  | otherwise = stepOne config $ stepMany config (days - 1) model

-- | Reads the 'Config' from a user in 'IO' action.
readConfig :: IO Config
readConfig = do
  putStrLn "Please enter random seed:"
  (_seed :: Int) <- readLn
  putStrLn "Please enter grid size:"
  (_size :: Int) <- readLn
  putStrLn "Please enter infection probability:"
  (_p :: Double) <- readLn
  putStrLn "Please enter incubation period length:"
  (_incubation :: Int) <- readLn
  putStrLn "Please enter symptomatic period length:"
  (_symptoms :: Int) <- readLn
  putStrLn "Please enter immunity period length:"
  (_immunity :: Int) <- readLn
  return $ Config _seed _size _p _incubation _symptoms _immunity

-- | Simulates one step of simulation in interaction with user in 'IO'.
simulateOne :: Config -> Model -> IO ()
simulateOne config model = do
  cmd <- getLine
  case cmd of
    "" -> do
      clearScreen
      printGrid config model
      simulateOne config $ stepOne config model
    "exit" -> return ()
    _ -> do
      putStrLn "unknown command"
      simulateOne config model

-- | Simulates a process of 'Model' evolution with user interaction.
simulateMany :: IO ()
simulateMany = do
  config <- readConfig
  simulateOne config $ patientZeroCase config
