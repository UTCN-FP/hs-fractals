{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bmp
import Julia (julia)
import Mandelbrot

import qualified Data.ByteString.Builder as BSB
import qualified System.Environment as ENV
import System.IO

data Fractal = Mandelbrot | Julia deriving (Read, Show, Eq)

rotateR :: Int -> [a] -> [a]
rotateR n l =
  drop n l ++ take n l

alternating = take 100 $ cycle [Pixel 0xff 0 0, Pixel 0 0xff 0, Pixel 0 0 0xff]

redRow = ImgRow (replicate 6 $ Pixel 0xff 0 0)

greenRow = ImgRow (replicate 6 $ Pixel 0 0xff 0)

blueRow = ImgRow (replicate 6 $ Pixel 0 0 0xff)

gradient :: Int -> Int -> BmpImg
gradient w h =
  let gen :: Int -> Int -> [[Pixel]]
      gen w h = do
        x <- [0 .. w -1]
        return [Pixel (fromIntegral (255 - x * 2)) (fromIntegral (255 - y * 2)) 0 | y <- [0 .. h -1]]
   in BmpImg $ map ImgRow (gen w h)

alternatingPixels = (BmpImg $ map ImgRow $ take 100 $ iterate (rotateR 1) alternating)

fractals = ["julia", "mandelbrot"]
juliaCfg w h iters = julia w h iters (-0.7) 0.27015 4.0 1

mandelbrotCfg w h iters = mandelbrot 0x3698217309173 w h iters (-0.5557506) (-0.55560) 0.001 1000

writeFractal :: Handle -> Fractal -> Int -> Int -> Int -> IO ()
writeFractal hFile fractal w h iters = 
  let imgFn = case fractal of
              Mandelbrot -> mandelbrotCfg
              Julia -> juliaCfg
      img = imgFn w h iters
  in do
    BSB.hPutBuilder hFile $ createBitmap img

procArgs :: [String] -> (Fractal, String, Int, Int, Int)
procArgs (nameS : widthS : heightS : itersS : _) = 
  let name = read nameS :: Fractal
      width = read widthS :: Int
      height = read heightS :: Int
      iters = read itersS :: Int
  in
    (name, nameS ++ ".bmp", width, height, iters)
procArgs _ = (Julia, "fractal.bmp", 100, 100, 255)

main :: IO ()
main = do
  args <- ENV.getArgs
  let (fractal, fileName, width, height, iters) = procArgs args
  hFile <- openBinaryFile fileName WriteMode
  writeFractal hFile fractal width height iters
  hFlush hFile
  hClose hFile
