{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Random     hiding (fromList)
import           Data.List.Split
import           MachineLearning.Hopfield
import           Numeric.LinearAlgebra.HMatrix
import           System.Console.CmdArgs
import qualified Data.Vector.Storable as V

-- Height and widght of the patterns we are training on
width, height :: Int
width = 6
height = 7

patterns :: Matrix Double
patterns = fromRows [x, o]
  where
    x = fromList
        [1, -1, -1, -1, -1, 1,
         -1, 1, -1, -1, 1, -1,
         -1, -1, 1, 1, -1, -1,
         -1, -1, 1, 1,  -1, -1,
         -1, -1, 1, 1, -1, -1,
         -1, 1, -1, -1, 1, -1,
         1, -1, -1, -1, -1, 1]
    o = fromList
        [1 , 1, 1, 1, 1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , 1, 1, 1, 1, 1]

randomCorruption :: MonadRandom m => Double -> Vector Double -> m (Vector Double)
randomCorruption proportion pattern = liftM (pattern //) mutations
     where
       numMutations = (floor . (proportion *) . fromIntegral . size) pattern
       mutationStream = liftM2 zip
                        (getRandomRs (0, size pattern - 1))
                        (getRandomRs (-1.0 :: Double, 1.0 :: Double))
       mutations =
           liftM (take numMutations . map (second activity)) mutationStream

-- | Squared distance in L^2
squaredDistance :: Vector Double -> Vector Double -> Double
squaredDistance = norm_2 .* (-) where (.*) = (.) . (.) -- Gratuitously pointfree
{-squaredDistance v1 v2 = (V.sum $ V.zipWith (\x y -> (x - y)**2.0) v1 v2) ** 0.5-}

validate :: HopfieldNet -> Int -> Double -> Vector Double -> IO ()
validate trained iterations corruptionLevel pattern =
    do
      corrupted <- evalRandIO $ randomCorruption corruptionLevel pattern
      reproduction <- evalRandIO $ reproduce corrupted
      print ("Corruption error", squaredDistance corrupted pattern)
      print ("Reproduction error", squaredDistance pattern reproduction)

      print "Original"
      displayPattern pattern
      print "Corrupted"
      displayPattern corrupted
      print "Reproduction"
      displayPattern reproduction
    where
      reproduce = associate trained iterations

displayPattern :: Vector Double -> IO ()
displayPattern pattern =
    do
      putStrLn divider
      mapM_ printLine patternLines
      putStrLn divider
    where
      divider = replicate (width + 2) '-'
      patternLines = chunksOf width $ toList pattern
      printLine line = do
        putStr "|"
        mapM_ (putStr . repr) line
        putStrLn "|"
      repr el = if activity el <= 0 then " " else "X"

-- Command line parsing
data HopfieldArgs = HopfieldArgs { _numIterations  :: Int
                                 , _corruptionRate :: Double
                                 } deriving (Show, Data, Typeable)

runSimulation :: HopfieldArgs -> IO ()
runSimulation (HopfieldArgs numIterations corruptionRate) =
    do
      putStrLn "Training patterns"
      eachPattern displayPattern

      putStrLn "Validation"
      eachPattern validatePattern
      return ()
  where
    eachPattern = forM_ (toRows patterns)
    validatePattern = validate trained numIterations corruptionRate
    trained = initializeWith patterns

main :: IO ()
main = cmdArgs hopfieldArgs >>= runSimulation
  where
    hopfieldArgs = HopfieldArgs {
                     _numIterations = def &= explicit &= name "num_iterations"
                   , _corruptionRate = def &= explicit &= name "corruption_rate"
                   }
