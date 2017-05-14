module Dottyback where

import Graphics.Rendering.Cairo hiding (x, y)

{- TODO

Image 5: R arrow
Image 6, 6a: T arrow

-}

height_ :: Num a => a
height_ = 800

width_  :: Num a => a
width_  = 1280

boundingBox :: Rectangle
boundingBox = Rectangle (0, 0) (width_, height_)

renderPNG :: FilePath -> Int -> Int -> Render a -> IO ()
renderPNG f w h p = withImageSurface FormatRGB24 w h $ \surface -> do
    _ <- renderWith surface p
    surfaceWriteToPNG surface f

renderSVG :: FilePath -> Double -> Double -> Render a -> IO a
renderSVG f w h p = withSVGSurface f w h $ \surface -> do
    renderWith surface p

main :: IO ()
main = do
  let images = [ (pic, "foo")
               , (image1, "image1")
               , (image2, "image2")
               , (image3, "image3")
               , (image4, "image4")
               , (image5, "image5")
               , (image6, "image6")
               , (image6a, "image6a")
               , (image7 N, "image71N")
               , (image7 E, "image72E")
               , (image7 S, "image73S")
               , (image7 W, "image74W")
               , (image8 N, "image81N")
               , (image8 E, "image82E")
               , (image8 S, "image83S")
               , (image8 W, "image84W")
               ]

  flip mapM_ images $ \(p, f) -> do
    renderPNG ("Output/" ++ f ++ ".png") width_ height_ p
    renderSVG ("Output/" ++ f ++ ".svg") width_ height_ p

type Point  = (Double, Double)
type Vector = (Double, Double)
type Length = Double
type Color  = (Double, Double, Double)

data Text = Text
  { tStart  :: Point
  , tText   :: String
  , tSize   :: Length
  }

drawText :: Text -> Render ()
drawText t = do uncurry moveTo (tStart t)
                setFontSize (tSize t)
                showText (tText t)

data LineSegment = LineSegment
  { lsStart :: Point
  , lsEnd   :: Point
  }

along :: Double -> LineSegment -> Point
along d l = lsStart l .+ (d .* (lsEnd l .- lsStart l))

lLength :: LineSegment -> Double
lLength l = modulus (lsStart l .- lsEnd l)

data Rectangle = Rectangle
  { rTopLeft     :: Point
  , rBottomRight :: Point
  }

data RectangleColor = RectangleColor
  { rcRectangle :: Rectangle
  , rcColor     :: Color
  }

horizMidline :: Rectangle -> LineSegment
horizMidline r = LineSegment (midpoint (rTopLeft  r) (rBottomLeft  r))
                             (midpoint (rTopRight r) (rBottomRight r))

withinSquare :: (Double, Double) -> Square -> Point
withinSquare (x, y) (Square (x1, y1) (x2, y2) _) =
  (x1 + (x2 - x1) * x, y1 + (y2 - y1) * y)

withinRectangle :: (Double, Double) -> Rectangle -> Point
withinRectangle (x, y) (Rectangle (x1, y1) (x2, y2)) =
  (x1 + (x2 - x1) * x, y1 + (y2 - y1) * y)

rTopRight :: Rectangle -> Point
rTopRight (Rectangle (_, y1) (x2, _)) = (x2, y1)

rBottomLeft :: Rectangle -> Point
rBottomLeft (Rectangle (x1, _) (_, y2)) = (x1, y2)

rightHalf :: Rectangle -> Rectangle
rightHalf r = Rectangle (midpoint (rTopLeft r) (rTopRight r)) (rBottomRight r)

rCenter :: Rectangle -> Point
rCenter r = midpoint (rTopLeft r) (rBottomRight r)

rHeight :: Rectangle -> Length
rHeight (Rectangle (_, y1) (_, y2)) = y2 - y1

sLength :: Square -> Length
sLength (Square (_, y1) (_, y2) _) = y2 - y1

data Square = Square
  { topLeft     :: Point
  , bottomRight :: Point
  , color       :: Color
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

data Orientation = N | E | S | W deriving Enum

oAdd :: Orientation -> Orientation -> Orientation
oAdd o1 o2 = toEnum ((fromEnum o1 + fromEnum o2) `mod` 4)

data KernelOriented = KernelOriented
  { koCenter      :: Point
  , koRadius      :: Length
  , koOrientation :: Orientation
  , koData        :: [[Color]]
  }

kernelOriented :: Point -> Length -> Orientation -> KernelOriented
kernelOriented c r o = KernelOriented c r o colours
  where colours = [ [ y, y, y ]
                  , [ y, w, w ]
                  , [ g, g, w ] ]
        y = (0.8, 0.8, 0.8)
        g = (0.3, 0.3, 0.3)
        w = (1, 1, 1)

drawKernelOriented :: KernelOriented -> Render ()
drawKernelOriented k = mapM_ drawSquare squares
  where squares = do x <- [-1 .. 1]
                     y <- [-1 .. 1]

                     let (x', y') = case koOrientation k of
                           N -> (x, y)
                           E -> (y, -x)
                           S -> (-x, -y)
                           W -> (-y, x)

                     return (squareCenterRadiusColor
                          (koCenter k .+ ( fromIntegral x * koRadius k / 3 * 2
                                         , fromIntegral y * koRadius k / 3 * 2))
                          (koRadius k / 3)
                          (colours !! (x' + 1) !! (y' + 1)))
          where colours = koData k

data Arc = Arc
  { arcFrom      :: Point
  , arcTo        :: Point
  , arcCurvature :: Double
  }

midpoint :: Point -> Point -> Point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

squareCenter :: Square -> Point
squareCenter s = midpoint (topLeft s) (bottomRight s)

sCenterToTop :: Square -> Vector
sCenterToTop s = (0, y)
  where (_, y) = (topLeft s .- bottomRight s) ./ 2

kernelCenter :: Kernel3x3 -> Point
kernelCenter k = midpoint (kernel3x3TopLeft k) (kernel3x3BottomRight k)

koPixelRadius :: KernelOriented -> Length
koPixelRadius k = koRadius k / 3

kPixelRadius :: Kernel3x3 -> Length
kPixelRadius k = (fst (kernel3x3BottomRight k) - fst (kernel3x3TopLeft k)) / 6

kRadius :: Kernel3x3 -> Length
kRadius = (* 3) . kPixelRadius

squareCenterRadius :: Point -> Double -> Square
squareCenterRadius (cx, cy) r = Square (cx - r, cy - r) (cx + r, cy + r) (1,1,1)

squareCenterRadiusColor :: Point -> Double -> Color -> Square
squareCenterRadiusColor (cx, cy) r c =
  Square (cx - r, cy - r) (cx + r, cy + r) c

kernel3x3CenterRadius :: Point -> Double -> Kernel3x3
kernel3x3CenterRadius (cx, cy) r = Kernel3x3 (cx - r, cy - r) (cx + r, cy + r)

gPlaneCenterRadius :: Point -> Double -> GPlane
gPlaneCenterRadius p r = GPlane p r (r / 2.5)

gPlaneTop :: GPlane -> Square
gPlaneTop g = t
  where (_, _, t, _) = gPlane4Squares g

data F = F
  { fCenter :: Point
  , fUp     :: Vector
  , fColor  :: Color
  }

data FShadow = FShadow
  { fsF         :: F
  , fsShadowDir :: Vector
  }

drawSquare :: Square -> Render ()
drawSquare (Square (x1, y1) (x2, y2) (c1, c2, c3)) = do
  moveTo x1 y1
  lineTo x2 y1
  lineTo x2 y2
  lineTo x1 y2
  closePath
  setFillRule FillRuleWinding
  setSourceRGB c1 c2 c3
  fillPreserve
  setSourceRGB 0 0 0
  stroke

drawRectangle :: Rectangle -> Render ()
drawRectangle (Rectangle (x1, y1) (x2, y2)) = do
  moveTo x1 y1
  lineTo x2 y1
  lineTo x2 y2
  lineTo x1 y2
  closePath
  stroke

drawRectangleColor :: RectangleColor -> Render ()
drawRectangleColor (RectangleColor (Rectangle (x1, y1) (x2, y2)) (c1, c2, c3)) = do
  moveTo x1 y1
  lineTo x2 y1
  lineTo x2 y2
  lineTo x1 y2
  closePath
  setFillRule FillRuleWinding
  setSourceRGB c1 c2 c3
  fillPreserve
  setSourceRGB 0 0 0
  stroke

gPlane4Squares :: GPlane -> (Square, Square, Square, Square)
gPlane4Squares g =
  ( gPlane1Square g W
  , gPlane1Square g E
  , gPlane1Square g N
  , gPlane1Square g S
  )

gPlane1Square :: GPlane -> Orientation -> Square
gPlane1Square (GPlane t h r) o = flip squareCenterRadius r . (t .+) $ case o of
  N -> (0, -h)
  E -> (h, 0)
  S -> (0, h)
  W -> (-h, 0)

drawGPlane :: GPlane -> Render ()
drawGPlane g = mapM_ drawSquare [a, b, c, d]
  where (a, b, c, d) = gPlane4Squares g

drawArc :: Arc -> Render ()
drawArc (Arc p1 p2 c) = do
  if c == 0 then
    do uncurry moveTo p1
       uncurry lineTo p2
  else uncurry arc cv radius theta1 theta2
  stroke
  where mid      = midpoint p1 p2
        d        = p2 .- p1
        r        = modulus d / 2
        radius   = r / 2 * (1 / c + c)
        tr       = radius - c * r
        dc       = (1 / 2 * tr / r) .* rotate90 d
        cv       = mid .+ dc
        theta1   = angle (p1 .- cv)
        theta2   = angle (p2 .- cv)

drawF :: F -> Render ()
drawF f = do
  uncurry moveTo (head fPathShrunk)
  mapM_ (uncurry lineTo) (tail fPathShrunk)
  closePath
  setFillRule FillRuleWinding
  setSourceRGB c1 c2 c3
  fillPreserve
  setSourceRGB 0 0 0
  stroke
  where fPath  = [ (0, 0)
                 , (3, 0)
                 , (3, 1)
                 , (1, 1)
                 , (1, 2)
                 , (2, 2)
                 , (2, 3)
                 , (1, 3)
                 , (1, 5)
                 , (0, 5)
                 ]
        fPathShrunk = map ((.+ fCenter f)
                           .
                           (\(x, y) -> (tx1 * x + tx2 * y,
                                       ty1 * x + ty2 * y))
                           .
                           (\(x, y) -> (x - 3 / 5, y - 1))
                           .
                           (\(x, y) -> (x * 2 / 5, y * 2 / 5)))
                      fPath
        r = fUp f
        (tx2, ty2) = (0, 0) .- r
        (tx1, ty1) = (ty2, -tx2)
        (c1, c2, c3) = fColor f

drawFShadow :: FShadow -> Render ()
drawFShadow fs = do
  drawF (F (fCenter (fsF fs) .+ fsShadowDir fs) (fUp (fsF fs)) (0,0,0))
  drawF (fsF fs)

modulus :: Vector -> Double
modulus (x, y) = sqrt (x * x + y * y)

(.*) :: Double -> Vector -> Vector
(.*) k (x, y) = (k * x, k * y)

(./) :: Vector -> Double -> Vector
(./) (x, y) k = (x / k, y / k)

(.-) :: Point -> Point -> Vector
(.-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(.+) :: Point -> Vector -> Point
(.+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

rotate90 :: Vector -> Vector
rotate90 (x, y) = (-y, x)

rotate270 :: Vector -> Vector
rotate270 = rotate90 . rotate90 . rotate90

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

initR :: Render ()
initR = do
  setSourceRGB 1 1 1
  rectangle 0 0 width_ height_
  fill

  setSourceRGB 0 0 0
  setLineWidth 1

pic :: Render ()
pic = do
  initR

  let gPlane2     = gPlaneCenterRadius (0.7 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 3.5)
      square      = squareCenterRadius (0.15 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 3.5 / 2.5)
      kernelO     = kernelOriented ((0.3, 0.3) `withinSquare` square)
                                   (0.2 * sLength square)
                                   N
      pixel       = squareCenterRadius ((0.3, 0.3)
                                        `withinSquare`
                                        gPlaneTop gPlane2)
                                       (0.2 / 3 * sLength square)
      arc_        = Arc (koCenter kernelO)
                        (squareCenter pixel)
                        0.3

      label       = Text (midpoint (arcFrom arc_) (arcTo arc_))
                         "T∘Φ"
                         (height_ / 20)


  drawGPlane         gPlane2
  drawRectangle      boundingBox
  drawSquare         square
  drawKernelOriented kernelO
  drawSquare         pixel
  drawArc            arc_
  drawText           label

image1 :: Render ()
image1 = do
  initR
  
  let square1     = squareCenterRadius (0.25 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 4)

      square2     = squareCenterRadius (0.75 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 4)

      kernel1     = kernelOriented ((0.3, 0.3) `withinSquare` square1)
                                   (0.15 * sLength square1)
                                   N
      kernel2     = kernelOriented ((0.6, 0.7) `withinSquare` square1)
                                   (0.15 * sLength square1)
                                   N

      pixel1      = squareCenterRadius ((0.3, 0.3) `withinSquare` square2)
                                       (koPixelRadius kernel1)

      pixel2      = squareCenterRadius ((0.6, 0.7) `withinSquare` square2)
                                       (koPixelRadius kernel1)

      arc1        = Arc (koCenter kernel1) (squareCenter pixel1) 0.3
      arc2        = Arc (koCenter kernel2) (squareCenter pixel2) 0.3


  drawSquare         square1
  drawSquare         square2
  drawKernelOriented kernel1
  drawKernelOriented kernel2
  drawSquare         pixel1
  drawSquare         pixel2
  drawArc            arc1
  drawArc            arc2

image2 :: Render ()
image2 = do
  initR

  let thisSquare t = squareCenterRadius (t `withinRectangle` boundingBox)
                                        (rHeight boundingBox / 6)

      sNW = thisSquare (0.3, 0.2)
      sNE = thisSquare (0.7, 0.2)
      sSE = thisSquare (0.7, 0.8)
      sSW = thisSquare (0.3, 0.8)

      i1 = RectangleColor (Rectangle (koCenter kernel1 .+ (koRadius kernel1 .* (-0.5, -1.7)))
                                     (koCenter kernel1 .+ (koRadius kernel1 .* (1.2, 3))))
                          (1, 0, 0)

      i2 = RectangleColor (Rectangle (koCenter kernel2 .+ (koRadius kernel2 .* (-0.5, -1.7)))
                                     (koCenter kernel2 .+ (koRadius kernel2 .* (1.2, 3))))
                          (1, 0, 0)

      kernel1 = kernelOriented ((0.2, 0.3) `withinSquare` sNW)
                               (0.15 * sLength sNW)
                               N

      kernel2 = kernelOriented ((0.8, 0.5) `withinSquare` sSW)
                               (0.15 * sLength sNW)
                               N

      pixel1 = squareCenterRadius ((0.2, 0.3) `withinSquare` sNE)
                                  (koPixelRadius kernel1)

      pixel2 = squareCenterRadius ((0.8, 0.5) `withinSquare` sSE)
                                  (koPixelRadius kernel2)

      arc1        = Arc (koCenter kernel1) (squareCenter pixel1) 0.3
      arc2        = Arc (koCenter kernel2) (squareCenter pixel2) 0.3

  mapM_ drawSquare [sNW, sNE, sSE, sSW]
  mapM_ drawSquare [pixel1, pixel2]
  mapM_ drawRectangleColor [i1, i2]
  mapM_ drawKernelOriented [kernel1, kernel2]
  mapM_ drawArc    [arc1, arc2]

image3 :: Render ()
image3 = do
  initR

  let square1     = squareCenterRadiusColor
                       (0.25 `along` horizMidline boundingBox)
                       (rHeight boundingBox / 4)
                       (0.2, 0.2, 0.2)

      square2     = squareCenterRadiusColor
                       (0.75 `along` horizMidline boundingBox)
                       (rHeight boundingBox / 4)
                       (0.2, 0.2, 0.2)

      kernel1     = kernelOriented ((0.3, 0.3) `withinSquare` square2)
                                   (0.15 * sLength square2)
                                   S

      pixel1      = squareCenterRadiusColor
                       ((0.3, 0.3) `withinSquare` square1)
                       (koPixelRadius kernel1)
                       (1, 1, 1)

      arc1        = Arc (squareCenter pixel1) (koCenter kernel1) (0.3)

  drawSquare         square1
  drawSquare         square2
  drawKernelOriented kernel1
  drawSquare         pixel1
  drawArc            arc1
  
image4 :: Render ()
image4 = do
  initR

  let thisSquare t = squareCenterRadius (t `withinRectangle` boundingBox)
                                        (rHeight boundingBox / 6)

      sNW = thisSquare (0.3, 0.2)
      sNE = thisSquare (0.7, 0.2)
      sSE = thisSquare (0.7, 0.8)
      sSW = thisSquare (0.3, 0.8)

      i1 = RectangleColor (Rectangle (koCenter kernel1 .+ (koRadius kernel1 .* (-0.5, -1.7)))
                                     (koCenter kernel1 .+ (koRadius kernel1 .* (1.2, 2))))
                          (1, 0, 0)

      p = (0.7, 0.2) `withinSquare` sSW

      i2 = RectangleColor (Rectangle (p .+ (koRadius kernel2 .* (-2, -0.5)))
                                     (p .+ (koRadius kernel2 .* (1.7, 1.2))))
                          (1, 0, 0)


      kernel1 = kernelOriented ((0.2, 0.3) `withinSquare` sNW)
                               (0.15 * sLength sNW)
                               N

      kernel2 = kernelOriented ((0.7, 0.2) `withinSquare` sSW)
                               (0.15 * sLength sNW)
                               N

      pixel1 = squareCenterRadius ((0.2, 0.3) `withinSquare` sNE)
                                  (koPixelRadius kernel1)

      pixel2 = squareCenterRadius ((0.7, 0.2) `withinSquare` sSE)
                                  (koPixelRadius kernel2)

      arc1        = Arc (koCenter kernel1) (squareCenter pixel1) 0.3
      arc2        = Arc (koCenter kernel2) (squareCenter pixel2) 0.3

      kernel      = KernelOriented (rCenter boundingBox)
                                   (koRadius kernel1)
                                   N
                                   [ [ w, g, w ]
                                   , [ g, y, g ]
                                   , [ w, g, w ] ]
        where y = (0.8, 0.8, 0.8)
              g = (0.3, 0.3, 0.3)
              w = (1, 1, 1)


  mapM_ drawSquare [sNW, sNE, sSE, sSW]
  mapM_ drawSquare [pixel1, pixel2]
  mapM_ drawRectangleColor [i1, i2]
  mapM_ drawKernelOriented [kernel1, kernel2]
  drawKernelOriented kernel
  mapM_ drawArc    [arc1, arc2]

imageG :: (Vector -> Vector) -> (Vector -> Vector) -> Render ()
imageG tt ff = do
  initR

  let gPlane1     = gPlaneCenterRadius (0.25 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 5)

      gPlane2     = gPlaneCenterRadius (0.75 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 5)


      (l1, r1, t1, b1) = gPlane4Squares gPlane1
      (l2, r2, t2, b2) = gPlane4Squares gPlane2

      fHeight      = sCenterToTop l1 ./ 2

      f x          = FShadow (F (squareCenter x) fHeight (1, 0, 0))
                             (d ./ 25)
        where d = squareCenter x .- center gPlane1

      g x          = FShadow (F (squareCenter x .+ tt d)
                                (ff fHeight)
                                (1, 0, 0))
                             (d ./ 25)
        where d = squareCenter x .- center gPlane2



  mapM_ drawGPlane [gPlane1, gPlane2]
  mapM_ drawFShadow (map f [l1, r1, t1, b1])
  mapM_ drawFShadow (map g [l2, r2, t2, b2])


image5 :: Render ()
image5 = imageG (const (0,0)) rotate270

image6 :: Render ()
image6 = imageG (\d -> d ./ 7) id

image6a :: Render ()
image6a = imageG (\d -> rotate90 (d ./ 7)) id

image7 :: Orientation -> Render ()
image7 o = do
  initR

  let gPlane       = gPlaneCenterRadius (0.7 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 3.5)

      (l, r, t, b) = gPlane4Squares gPlane

      pixelSquare = case o of
        N -> t
        E -> r
        S -> b
        W -> l

      square      = squareCenterRadius (0.15 `along` horizMidline boundingBox)
                                       (rHeight boundingBox / 3.5 / 2.5)
      kernelO     = kernelOriented ((0.3, 0.3) `withinSquare` square)
                                   (0.2 * sLength square)
                                   o
      pixel       = squareCenterRadius ((0.3, 0.3)
                                        `withinSquare`
                                        pixelSquare)
                                       (0.2 / 3 * sLength square)
      arc_        = Arc (koCenter kernelO)
                        (squareCenter pixel)
                        0.3


  drawGPlane         gPlane
  drawRectangle      boundingBox
  drawSquare         square
  drawKernelOriented kernelO
  drawSquare         pixel
  drawArc            arc_

image8 :: Orientation -> Render ()
image8 o = do
  initR

  let gPlane1 = gPlaneCenterRadius (0.25 `along` horizMidline boundingBox)
                (rHeight boundingBox / 5)

      gPlane2 = gPlaneCenterRadius (0.75 `along` horizMidline boundingBox)
               (rHeight boundingBox / 5)

      pixelSquare = gPlane1Square gPlane2 o

      y = (0.8, 0.8, 0.8)
      g = (0.3, 0.3, 0.3)
      w = (1, 1, 1)

      kernel o' colours =
        KernelOriented ((0.3, 0.3) `withinSquare`p)
                       (0.2 * sLength p)
                       o
                       colours
        where p = gPlane1Square gPlane1 (oAdd o o')

      kernels = [kernelOl, kernelOr, kernelOt, kernelOb]

      kernelOl    = kernel N [ [ y, y, y ]
                             , [ y, w, w ]
                             , [ g, g, w ] ]

      kernelOr    = kernel E [ [ w, w, w ]
                             , [ y, g, g ]
                             , [ w, w, w ] ]

      kernelOt    = kernel S [ [ g, w, w ]
                             , [ y, g, g ]
                             , [ w, w, y ] ]

      kernelOb    = kernel W [ [ y, w, y ]
                             , [ w, g, w ]
                             , [ g, g, g ] ]

      pixel       = squareCenterRadius ((0.3, 0.3)
                                        `withinSquare`
                                        pixelSquare)
                                       (0.2 / 3 * sLength pixelSquare)

      arcs = map (\x -> Arc (koCenter x) (squareCenter pixel) 0.4)
                 kernels

  drawGPlane         gPlane1
  drawGPlane         gPlane2
  drawRectangle      boundingBox
  mapM_ drawKernelOriented kernels
  drawSquare         pixel
  mapM_ drawArc      arcs

