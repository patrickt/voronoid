{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

module Main (main) where

import           Control.Effect
import           Control.Effect.Random
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Foldable
import           Data.Geometry.Point (Point)
import qualified Data.Geometry.Point as Point
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Graphics.Gloss as Gloss
import qualified Data.List.NonEmpty as NonEmpty
import qualified Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer as Delaunay
import qualified Algorithms.Geometry.DelaunayTriangulation.Types as Delaunay
import           Data.Ext as Ext
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           System.IO

maxX, maxY :: Double
(maxX, maxY) = (300, 300)

randomPoint :: MonadRandom m => m (Point 2 Double)
randomPoint = Point.point2 <$> getRandomR (0, maxX) <*> getRandomR (0, maxY)

drawPoints :: Delaunay.Triangulation Char Double -> Gloss.Picture
drawPoints tri = Gloss.translate (-150) (-150) (allLines <> allPoints)
  where
    vertices = Delaunay._vertexIds tri
    origin = Gloss.circleSolid 5
    allPoints = origin <> Map.foldMapWithKey pointPic vertices
    allLines = foldMap linePic (Delaunay.triangulationEdges tri)

    linePic (a :+ _, b :+ _) = let
      look = bimap realToFrac realToFrac . Point._point2
      in Gloss.line [look a, look b]

    pointPic (Point.Point2 x y) idx
      = Gloss.translate (realToFrac x) (realToFrac y)
      . Gloss.scale 0.1 0.1
      . Gloss.text
      $ show idx

main :: IO ()
main = runM . evalRandomIO $ do
  points <- replicateM 5 randomPoint
  let tagged = zipWith (:+) points ['a'..]
  let disp  = Gloss.InWindow "Logos v0.1" (round maxX, round maxY) (400, 400)
  traverse_ (liftIO . print) tagged
  let triangles = Delaunay.delaunayTriangulation (NonEmpty.fromList tagged)
  liftIO $ print triangles
