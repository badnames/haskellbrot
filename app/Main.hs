module Main where

import Lib
import Data.Complex
import Graphics.Image

isInMandelbrotSet :: Complex Double -> Complex Double -> Int -> Int
isInMandelbrotSet c z 0 = 0
isInMandelbrotSet c z itterations | (Data.Complex.magnitude z)  < 4 = isInMandelbrotSet c (z**2 + c) (itterations - 1)
                                  | (Data.Complex.magnitude z) >= 4 = itterations

calculatePixel :: Int -> Int -> Pixel RGB Double
calculatePixel x y = PixelRGB  0 0 value where
    real = (fromIntegral y / 250) - 2
    imag = (fromIntegral x / 250) - 1
    value = ((fromIntegral (isInMandelbrotSet (real :+ imag) 0 1000)) / 1000)

main = do
    let image = makeImageR VS (500, 750) (\(x, y) -> calculatePixel x y)
    writeImageExact PNG [] "output.png" image