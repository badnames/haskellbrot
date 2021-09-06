module Main where

import Lib
import Data.Complex

isInMandelbrotSet :: Complex Double -> Complex Double -> Int -> Bool
isInMandelbrotSet c z 0 = True
isInMandelbrotSet c z itterations | (magnitude z)  < 4 = isInMandelbrotSet c (z**2 + c) (itterations - 1)
                             | (magnitude z) >= 4 = False

main :: IO ()
main = do
    print (isInMandelbrotSet (-1) 0 1000)