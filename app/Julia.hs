{-# LANGUAGE BangPatterns #-}
module Julia where

import Control.Parallel.Strategies

import Bmp
import Color

import Data.Bits

julia :: Int -> Int -> Int -> Float -> Float -> Float -> Float -> BmpImg
julia w h nrIters cx cy zAbs zoom =
  let genPixel :: Int -> Int -> Pixel
      genPixel x y =
        let xf = fromIntegral x
            yf = fromIntegral y
            wf = fromIntegral w
            hf = fromIntegral h
            zx = 1.5 * (xf - wf / 2) / (0.5 * zoom * wf) :: Float
            zy = (yf - hf / 2) / (0.5 * zoom * hf) :: Float

            iterFn :: (Float, Float, Int) -> (Float, Float, Int)
            iterFn (!zx, !zy, !it) = (zx ^ 2 - zy ^ 2 + cx, 2.0 * zx * zy + cy, it - 1)

            start = (zx, zy, nrIters)
            iters = dropWhile (\(!a, !b, !it) -> ((a ^ 2 + b ^ 2) < zAbs) && it > 0) $ iterate iterFn start

            i = case iters of
              (_, _, its) : _ -> its `mod` 255
              _ -> 0

            cr = fromIntegral $ i `shiftL` if i < 128 then 2 else 4
            cg = fromIntegral $ i `shiftL` if i < 128 then 3 else 2
            cb = fromIntegral $ i `shiftL` if i < 128 then 4 else 5
         in Pixel cr cg cb

      gen :: Int -> Int -> [[Pixel]]
      gen w h = [[genPixel x y | x <- [0 .. w -1]]| y <- reverse [0 .. h -1]] `using` (parList rdeepseq)
   in BmpImg $ map ImgRow (gen w h)