module DSP
  ( fft,
    ifft,
    convolve,
    createLowPassFilter,
    createBandPassFilter,
    createHighPassFilter,
    quadtratureDemodulate,
  )
where

import qualified Data.Complex as C
import qualified Data.Vector.Unboxed as VU

fft :: VU.Vector Double -> VU.Vector C.Complex
ifft :: VU.Vector (C.Complex Double) -> VU.Vector Double
convolve :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
createLowPassFilter :: Int -> Double -> Double -> VU.Vector Double
createBandPassFilter :: Int -> Double -> Double -> Double -> VU.Vector Double
createHighPassFilter :: Int -> Double -> Double -> VU.Vector Double
quadtratureDemodulate :: VU.Vector Double -> Double -> Double -> (VU.Vector Double, VU.Vector Double)
