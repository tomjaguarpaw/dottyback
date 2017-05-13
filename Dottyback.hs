module Dottyback where

import Graphics.Rendering.Cairo


main = withImageSurface FormatRGB24 300 300 $ \surface -> do
    renderWith surface pic
    surfaceWriteToPNG surface "/tmp/foo.png"

type Point  = (Double, Double)
type Vector = (Double, Double)

data Square = Square
  { topLeft     :: Point
  , bottomRight :: Point
  }

data GPlane = GPlane
  { center      :: Point
  , hubDistance :: Double
  , planeRadius :: Double
  }

data Kernel3x3 = Kernel3x3
  { kernel3x3TopLeft     :: Point
  , kernel3x3BottomRight :: Point
  }

data Arc = Arc
  { arcFrom      :: Point
  , arcTo        :: Point
  , arcCurvature :: Double
  }

midpoint :: Point -> Point -> Point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

squareCentre :: Square -> Point
squareCentre s = midpoint (topLeft s) (bottomRight s)

kernelCentre :: Kernel3x3 -> Point
kernelCentre k = midpoint (kernel3x3TopLeft k) (kernel3x3BottomRight k)

squareCenterRadius :: Point -> Double -> Square
squareCenterRadius (cx, cy) r = Square (cx - r, cy - r) (cx + r, cy + r)
  where d = r / 2

kernel3x3CenterRadius :: Point -> Double -> Kernel3x3
kernel3x3CenterRadius (cx, cy) r = Kernel3x3 (cx - r, cy - r) (cx + r, cy + r)
  where d = r / 2 

drawSquare :: Square -> Render ()
drawSquare (Square (x1, y1) (x2, y2)) = do
  moveTo x1 y1
  lineTo x2 y1
  lineTo x2 y2
  lineTo x1 y2
  lineTo x1 y1
  stroke

gPlane4Squares :: GPlane -> (Square, Square, Square, Square)
gPlane4Squares (GPlane (cx, cy) h r) =
  ( squareCenterRadius (cx - h, cy) r
  , squareCenterRadius (cx + h, cy) r
  , squareCenterRadius (cx, cy - h) r
  , squareCenterRadius (cx, cy + h) r
  )

drawGPlane :: GPlane -> Render ()
drawGPlane g = mapM_ drawSquare [a, b, c, d]
  where (a, b, c, d) = gPlane4Squares g

drawArc :: Arc -> Render ()
drawArc (Arc p1 p2 c) = do
  if c == 0 then
    do uncurry moveTo p1
       uncurry lineTo p2
  else arc cx cy radius theta1 theta2
  stroke
  where mid      = midpoint p1 p2
        d        = p2 .- p1
        r        = modulus d / 2
        radius   = r / 2 * (1 / c + c)
        tr       = radius - c * r
        dc       = (1 / 2 * tr / r) .* rotate90 d
        (cx, cy) = mid .+ dc
        theta1   = angle (p1 .- (cx, cy))
        theta2   = angle (p2 .- (cx, cy))

modulus :: Vector -> Double
modulus (x, y) = sqrt (x * x + y * y)

(.*) :: Double -> Vector -> Vector
(.*) k (x, y) = (k * x, k * y)

(./) :: Double -> Vector -> Vector
(./) k (x, y) = (k / x, k / y)

(.-) :: Point -> Point -> Vector
(.-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(.+) :: Point -> Vector -> Point
(.+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

rotate90 :: Vector -> Vector
rotate90 (x, y) = (-y, x)

angle :: Vector -> Double
angle (x, y) = atan2 y x

kernel3x39Squares :: Kernel3x3 -> ( (Square, Square, Square)
                                  , (Square, Square, Square)
                                  , (Square, Square, Square) )
kernel3x39Squares (Kernel3x3 (x1, y1) (x2, y2)) =
  ( ( squareCenterRadius (midx - dx, midy - dy) (dx / 2)
    , squareCenterRadius (midx     , midy - dy) (dx / 2)
    , squareCenterRadius (midx + dx, midy - dy) (dx / 2) )
  , ( squareCenterRadius (midx - dx, midy     ) (dx / 2)
    , squareCenterRadius (midx     , midy     ) (dx / 2)
    , squareCenterRadius (midx + dx, midy     ) (dx / 2) )
  , ( squareCenterRadius (midx - dx, midy + dy) (dx / 2)
    , squareCenterRadius (midx     , midy + dy) (dx / 2)
    , squareCenterRadius (midx + dx, midy + dy) (dx / 2) )
  )
  where (midx, midy) = midpoint (x1, y1) (x2, y2)
        dx      = (x2 - x1) / 3
        dy      = (y2 - y1) / 3

drawKernel3x3 :: Kernel3x3 -> Render ()
drawKernel3x3 k = mapM_ draw3Squares [a, b, c]
  where (a, b, c) = kernel3x39Squares k
        draw3Squares (d, e, f) = mapM_ drawSquare [d, e, f]

pic :: Render ()
pic = do
  setSourceRGB 1 1 1
  rectangle 0 0 300 300
  fill
  

  setSourceRGB 0 0 0
  setLineWidth 1
  
  let gPlane = GPlane (100, 50) 30 5
      kernel = kernel3x3CenterRadius (200, 200) 15

      (_, _, _, s) = gPlane4Squares gPlane

  drawSquare (squareCenterRadius (20, 20) 5)
  drawGPlane gPlane
  drawKernel3x3 kernel

  drawArc (Arc (squareCentre s) (kernelCentre kernel) 0.5)

  uncurry moveTo (midpoint (squareCentre s) (kernelCentre kernel))
  showText "T∘Φ"
