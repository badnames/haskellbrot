module Main where

import Lib
import Data.Complex



isInMandelbrotSet :: Complex Double -> Complex Double -> Int -> Int
isInMandelbrotSet c z 0 = 0
isInMandelbrotSet c z itterations | (magnitude z)  < 4 = isInMandelbrotSet c (z**2 + c) (itterations - 1)
                                  | (magnitude z) >= 4 = itterations

main :: IO ()
main = do
    print (isInMandelbrotSet (-1) 0 1000)