module Color where

import qualified Data.Word as W

type HSL = (Float, Float, Float)
type RGB = (W.Word8, W.Word8, W.Word8)

hslToRgb :: HSL -> RGB
hslToRgb (_, 0.0, _) = (255, 255, 255)
hslToRgb (h, s, l) =
  let
    q = if l < 0.5 then l * s else l + s - l * s
    p = 2 * l - q
    r = (hueToRgb p q h) + (1.0 / 3.0)
    g = hueToRgb p q h
    b = (hueToRgb p q h) - (1.0 / 3.0)
  in
    (round $ r * 255.0, round $ g * 255.0, round $ b * 255.0)

hueToRgb :: Float -> Float -> Float -> Float
hueToRgb p q t =
  inner p q (if t < 0 then t + 1.0 else t - 1.0) where
    inner p q t
      | t < (1.0 / 6.0) = p + (q - p) * 6.0 * t
      | (t < (1.0 / 2.0)) = q
      | (t < (2.0 / 3.0)) = p + (q - p) * (2.0 / 3.0 - t) * 6.0
      | otherwise = p
    