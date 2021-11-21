{-# LANGUAGE BangPatterns #-}

module Mandelbrot where

import Bmp
import Color
import Control.Parallel.Strategies
import Data.Bits
import qualified Data.Word as W

nextXor :: W.Word64 -> W.Word64
nextXor w =
  let a = w `xor` (w `shiftR` 12)
      b = a `xor` (a `shiftR` 25)
      c = b `xor` (b `shiftR` 27)
   in c * 0x2545F4914F6CDD1D

nextFloat :: W.Word64 -> (Float, W.Word64)
nextFloat w = (fromIntegral w / fromIntegral maxW, nextXor w) where maxW = maxBound :: W.Word64

paint :: Float -> Float -> (W.Word8, W.Word8, W.Word8)
paint !r !n
  | r > 4.0 = hslToRgb (n / 800.0 * r, 1.0, 0.5)
  | otherwise = (255, 255, 255)

mandelbrotIter :: Float -> Float -> Int -> (Float, Int)
mandelbrotIter px py maxIter =
  getRes $ dropWhile (\(_, _, !xx, !yy, _, !i) -> (xx + yy <= 4.0) && (i < maxIter)) $ iterate nextIter (0.0, 0.0, 0.0, 0.0, 0.0, 0)
  where
    nextIter :: (Float, Float, Float, Float, Float, Int) -> (Float, Float, Float, Float, Float, Int)
    nextIter (!x, !y, _, _, _, !i) =
      let nextXX = x * x
          nextYY = y * y
          nextXY = x * y
          nextX = nextXX - nextYY + px
          nextY = 2 * nextXY + py
       in (nextX, nextY, nextXX, nextYY, nextXY, i + 1)
    getRes ((_, _, !xx, !yy, _, !i) : _) = (xx + yy, i)

mandelbrot :: W.Word64 -> Int -> Int -> Int -> Float -> Float -> Float -> Int -> BmpImg
mandelbrot seed w h samples px py size maxIter =
  let genPixel :: Int -> Int -> W.Word64 -> Pixel
      genPixel x y pSeed =
        let genSample :: (W.Word32, W.Word32, W.Word32, W.Word64) -> (W.Word32, W.Word32, W.Word32, W.Word64)
            genSample (!cr, !cg, !cb, !cSeed) =
              let (rnd1, seed1) = nextFloat cSeed
                  (rnd2, seed2) = nextFloat seed1
                  nx = size * (fromIntegral x + rnd1) / fromIntegral w + px
                  ny = size * (fromIntegral y + rnd2) / fromIntegral h + py
                  (mRes, mIter) = mandelbrotIter nx ny maxIter
                  (!pr, !pg, !pb) = paint mRes (fromIntegral mIter)
               in (cr + fromIntegral pr, cg + fromIntegral pg, cb + fromIntegral pb, seed2)
            (sr, sg, sb, _) : _ = drop samples $ iterate genSample (0, 0, 0, pSeed)
            dr = fromIntegral sr / fromIntegral samples :: Float
            dg = fromIntegral sg / fromIntegral samples :: Float
            db = fromIntegral sb / fromIntegral samples :: Float
         in Pixel (round dr) (round dg) (round db)
      pixels = [[genPixel x y (seed + fromIntegral (y * w + x)) | x <- [0 .. w -1]] | y <- reverse [0 .. h -1]] `using` (parList rdeepseq)
   in BmpImg $ map ImgRow pixels