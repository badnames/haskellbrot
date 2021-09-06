module Main where

import Lib
import Data.Complex
import Graphics.Image

width = 750
height = 500
scaleDivisor = 250

xOffset = 2
yOffset = 1

isInMandelbrotSet :: Complex Double -> Complex Double -> Int -> Int
isInMandelbrotSet c z 0 = 0
isInMandelbrotSet c z itterations | (Data.Complex.magnitude z)  < 4 = isInMandelbrotSet c (z**2 + c) (itterations - 1)
                                  | (Data.Complex.magnitude z) >= 4 = itterations

calculatePixel :: Int -> Int -> Pixel RGB Double
calculatePixel y x = PixelRGB  0 0 value where
    real = (fromIntegral x / scaleDivisor) - xOffset
    imag = (fromIntegral y / scaleDivisor) - yOffset
    value = ((fromIntegral (isInMandelbrotSet (real :+ imag) 0 1000)) / 1000)

main = do
    let image = makeImageR VS (height, width) (\(y, x) -> calculatePixel y x)
    writeImageExact PNG [] "output.png" image