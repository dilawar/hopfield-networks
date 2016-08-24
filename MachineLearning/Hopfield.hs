{-# LANGUAGE RecordWildCards #-}
module MachineLearning.Hopfield
    (HopfieldNet(..),
     initialize,
     initializeWith,
     activity,
     train,
     associate,
     (//),
     energy) where

import           Control.Monad
import           Control.Monad.Random hiding (fromList)
import           Foreign.Storable
import           Numeric.LinearAlgebra.HMatrix
import qualified Numeric.LinearAlgebra.Devel as LD
import           Numeric.LinearAlgebra.Data

data HopfieldNet = HopfieldNet { state   :: Vector Double
                               , weights :: Matrix Double
                               } deriving (Show)


activity :: (Floating a, Ord a) => a -> a
activity activation = if activation <= 0 then -1.0 else 1.0

initialize :: Int -> HopfieldNet
initialize n = HopfieldNet (fromList (replicate n 0)) ((n >< n) (repeat 0))

initializeWith :: Matrix Double -> HopfieldNet
initializeWith patterns = train state patterns
    where
      state = initialize (cols patterns)

(//)  :: Foreign.Storable.Storable a => Vector a -> [(Int, a)] -> Vector a
vec // mutations = LD.runSTVector $ LD.thawVector vec >>= mutate
    where
      mutate mv = forM_ mutations (modify mv) >> return mv
      modify mv (idx, value) = LD.modifyVector mv idx (const value)


update' :: HopfieldNet -> Int -> HopfieldNet
update' HopfieldNet{..} neuron = HopfieldNet newState weights
    where
      newState = state // [(neuron, activity activation)]
      activation = (toColumns weights !! neuron) <.> state

update :: MonadRandom m => HopfieldNet -> m HopfieldNet
update n =  liftM (update' n) $ getRandomR (0, (size . state) n - 1)

train :: HopfieldNet -> Matrix Double -> HopfieldNet
train HopfieldNet{..}  patterns = HopfieldNet state (add weights updates)
    where
      updates = build (n, n) weight'
      n = size state
      scalingFactor = 1.0 / fromIntegral (rows patterns)
      -- NOTE: build function called above truns index into Float type. Before
      -- indexing, they should be cast into Int
      weight' fi fj = weight (ceiling fi) (ceiling fj)
      weight :: Int -> Int -> Double
      weight i j = (toColumns patterns !! i) 
                     <.> (toColumns patterns !! j)
                      * scalingFactor

settle
  :: (Enum b, Num b, MonadRandom m) =>
     HopfieldNet -> b -> m HopfieldNet
settle net iterations = foldM (\state _ -> update state) net [1..iterations]

associate :: MonadRandom m => HopfieldNet -> Int -> Vector Double -> m (Vector Double)
associate net iterations pattern =
    liftM state $ settle (net { state = pattern }) iterations

energy :: HopfieldNet -> Double
energy HopfieldNet{..}  = -0.5 * ((weights #> state) <.> state)
