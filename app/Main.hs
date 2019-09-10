{-# LANGUAGE DataKinds, FlexibleContexts #-}

module Main (main) where

import           Control.Effect
import           Control.Effect.Random
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Random.Class
import           Data.Foldable
import           Data.Geometry.Point (Point)
import qualified Data.Geometry.Point as Point

maxX, maxY :: Int
(maxX, maxY) = (300, 300)

randomPoint :: MonadRandom m => m (Point 2 Int)
randomPoint = Point.point2 <$> getRandomR (0, maxX) <*> getRandomR (0, maxY)

main :: IO ()
main = runM . evalRandomIO $ do
  points <- replicateM 5 randomPoint
  traverse_ (liftIO . print) points
