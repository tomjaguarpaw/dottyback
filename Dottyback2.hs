{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dottyback2 where

import qualified Dottyback
import           Data.Monoid (Monoid, mempty, mappend)
import qualified Control.Monad.Trans.Writer as W
import           Graphics.Rendering.Cairo (Render)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Lens.Micro as L
import qualified Lens.Micro.Extras as L

data Length = Length { llength :: Double }

newtype M = M (Render ())

instance Monoid M where
  mempty = M (pure ())
  M x1 `mappend` M x2 = M (x1 >> x2)

newtype S a = S (W.Writer M a)
            deriving (Functor, Applicative, Monad)

data Point = Point
  { pX :: Double
  , pY :: Double
  }

data Vector = Vector
  { vX :: Double
  , vY :: Double
  }

vectorLength :: L.Lens' Vector Length
vectorLength = L.lens get set
  where get v   = Length (sqrt (vX v * vX v + vY v * vY v))
        set v l = v .* scale
          where scale = ld / get_v
                Length get_v = get v
                Length ld = l

data Direction = Direction Vector

data Circle = Circle
  { cCenter :: Point
  , cRadius :: Length
  }

data LineSegment = LineSegment
  { lStart :: Point
  , lEnd   :: Point
  }

(./) :: Vector -> Double -> Vector
v ./ l = Vector { vX = vX v / l, vY = vY v / l }

(.*) :: Vector -> Double -> Vector
v .* l = Vector { vX = vX v * l, vY = vY v * l }

(.+) :: Point -> Vector -> Point
p .+ v = Point { pX = pX p + vX v, pY = pY p + vY v }

(.-) :: Point -> Point -> Vector
p1 .- p2 = Vector { vX = pX p1 - pX p2, vY = pY p1 - pY p2 }

fromToLength :: Point -> Point -> Length -> Point
fromToLength p1 p2 l = p1 .+ L.set vectorLength l (p2 .- p1)

circleConnector :: Circle -> Circle -> LineSegment
circleConnector c1 c2 = LineSegment {
    lStart = fromToLength (cCenter c1) (cCenter c2) (cRadius c1)
  , lEnd   = fromToLength (cCenter c2) (cCenter c1) (cRadius c2)
  }

drawCircle :: Circle -> M
drawCircle c = M $ do
  Cairo.arc (pX center)
            (pY center)
            (llength (cRadius c))
            0
            (2 * 3.14)
  Cairo.stroke
  where center = cCenter c

drawLineSegment :: LineSegment -> M
drawLineSegment l = M $ do
  let Point x1 y1 = lStart l
      Point x2 y2 = lEnd   l

  Cairo.moveTo x1 y1
  Cairo.lineTo x2 y2
  Cairo.stroke

image1 :: M
image1 =
  let c1 = Circle (Point 50 50) (Length 10)
      c2 = Circle (Point 70 100) (Length 25)

      l = circleConnector c1 c2

  in mconcat [drawCircle c1, drawCircle c2, drawLineSegment l]

main :: IO ()
main = do
  let images = [ (image1, "new_image1") ]
      width :: Num a => a
      width = 200
      height :: Num a => a
      height = 200

  flip mapM_ images $ \(M p, f) -> do
    Dottyback.renderPNG ("Output/" ++ f ++ ".png") width height (Dottyback.initR >> p)
    Dottyback.renderSVG ("Output/" ++ f ++ ".svg") width height (Dottyback.initR >> p)
