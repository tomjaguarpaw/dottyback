{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

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

data Text = Text
  { tStart :: Point
  , tText  :: String
  , tSize  :: Length
  }

vectorLength :: L.Lens' Vector Length
vectorLength = L.lens get set
  where get v   = Length (sqrt (vX v * vX v + vY v * vY v))
        set v l = (l /. get v) *. v

vectorDirection :: L.Lens' Vector Direction
vectorDirection = L.lens get set
  where get = Direction
        set v d = (d ..- get v) `rotateTurns` v

rotateTurns :: Double -> Vector -> Vector
rotateTurns theta v = Vector { vX =   cos (twopi * theta) * vX v
                                    - sin (twopi * theta) * vY v
                             , vY =   sin (twopi * theta) * vX v
                                    + cos (twopi * theta) * vY v
                             }
  where twopi = 2 * 3.14159

rotate :: Double -> Direction -> Direction
rotate theta (Direction v) = Direction (rotateTurns theta v)

vectorNegate :: Vector -> Vector
vectorNegate v = Vector { vX = - vX v, vY = - vY v }

vectorDL :: Direction -> Length -> Vector
vectorDL (Direction v) l = L.set vectorLength l v

inDirection :: Length -> Direction -> Vector
inDirection = flip vectorDL

data Direction = Direction Vector

data Circle = Circle
  { cCenter :: Point
  , cRadius :: Length
  }

data Rectangle = Rectangle
  { rCenter     :: Point
  , rAxis       :: Vector
  , rHalfLength :: Length
  }

rAxisL :: L.Lens' Rectangle Vector
rAxisL = L.lens get set
  where get = rAxis
        set r a = r { rAxis = a }

data LineSegment = LineSegment
  { lStart :: Point
  , lEnd   :: Point
  }

class Divide a b c | b c -> a, a b -> c, a c -> b where
  (/.) :: a -> b -> c

instance Divide Length Length Double where
  Length x1 /. Length x2 = x1 / x2

instance Divide Vector Double Vector where
  v /. l = Vector { vX = vX v / l, vY = vY v / l }

class Multiply a where
  (*.) :: Double -> a -> a

instance Multiply Vector where
  l *. v = Vector { vX = vX v * l, vY = vY v * l }

instance Multiply Length where
  l *. (Length len) = Length (l * len)

(.+) :: Point -> Vector -> Point
p .+ v = Point { pX = pX p + vX v, pY = pY p + vY v }

(.-) :: Point -> Point -> Vector
p1 .- p2 = Vector { vX = pX p1 - pX p2, vY = pY p1 - pY p2 }

(..-) :: Direction -> Direction -> Double
Direction v1 ..- Direction v2 = thetaRadians / (2 * 3.14159)
  where dot = vX v1 * vX v2 + vY v1 * vY v2
        Length mod1 = L.view vectorLength v1
        Length mod2 = L.view vectorLength v2
        cosTheta = dot / (mod1 * mod2)
        thetaRadians = acos cosTheta

centerLineSegment :: LineSegment -> Point
centerLineSegment l = lStart l .+ (0.5 *. (lEnd l .- lStart l))

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

drawText :: Text -> M
drawText t = M $ do
  let start = tStart t
  let Length size = tSize t

  Cairo.moveTo (pX start) (pY start)
  Cairo.setFontSize size
  Cairo.showText (tText t)

drawRectangle :: Rectangle -> M
drawRectangle r = M $ do
  let a1 = rAxis r
      a2 = (L.set vectorLength (rHalfLength r)
            . rotateTurns 0.25
            . rAxis) r
      center = rCenter r

  onPoint Cairo.moveTo ((center .+ a1) .+ a2)
  onPoint Cairo.lineTo ((center .+ a1) .+ vectorNegate a2)
  onPoint Cairo.lineTo ((center .+ vectorNegate a1) .+ vectorNegate a2)
  onPoint Cairo.lineTo ((center .+ vectorNegate a1) .+ a2)
  Cairo.closePath
  Cairo.stroke

  where onPoint f p = f (pX p) (pY p)


-- | Approximate length
textLength :: Text -> Length
textLength t = (0.5 * fromIntegral (length (tText t))) *. tSize t

image1 :: M
image1 =
  let frame' = Rectangle { rCenter     = Point 250 250
                         , rAxis       = Vector 0 (-200)
                         , rHalfLength = Length 200
                         }

      frame = L.over (rAxisL.vectorDirection) (rotate 0.02) frame'

      c1 = Circle { cCenter = rCenter frame .+ (0.8 *. rAxis frame)
                  , cRadius = 0.1 *. L.view vectorLength (rAxis frame)
                  }
      c2 = Circle { cCenter = cCenter c1
                              .+ ((6 *. cRadius c1) `inDirection` down)
                              .+ ((3 *. cRadius c1) `inDirection` left)
                  , cRadius = 0.8 *. cRadius c1
                  }

      l = circleConnector c1 c2
      t = Text { tStart = centerLineSegment l
                          .+ (textLength t `inDirection` left)
               , tText = "(p, v)"
               , tSize = cRadius c1
               }

      up        = Direction (rAxis frame)
      right     = rotate 0.25 up
      down      = rotate 0.5  up
      left      = rotate 0.75 up

  in mconcat [ drawRectangle frame
             , drawCircle c1
             , drawCircle c2
             , drawLineSegment l
             , drawText t
             ]

main :: IO ()
main = do
  let images = [ (image1, "new_image1") ]
      width :: Num a => a
      width = 500
      height :: Num a => a
      height = 500

  flip mapM_ images $ \(M p, f) -> do
    Dottyback.renderPNG ("Output/" ++ f ++ ".png") width height (Dottyback.initR >> p)
    Dottyback.renderSVG ("Output/" ++ f ++ ".svg") width height (Dottyback.initR >> p)
