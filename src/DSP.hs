{- src/DSP.hs -}
module DSP
  ( fft,
    ifft,
    convolve,
    createLowPassFilter,
    createBandPassFilter,
    createHighPassFilter,
    quadratureDemodulate,
    -- additional exported utilities
    nextPowerOf2,
  )
where

import Data.Bits (popCount, shiftL)
import Data.Complex (Complex (..), realPart)
import Data.Vector.Generic (convert)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Numeric.FFT.Vector.Invertible as FFT

-- | find the next power of 2 greater than or equal to n
nextPowerOf2 :: Int -> Int
nextPowerOf2 n
  | n <= 0 = 1
  | isPowerOf2 n = n
  | otherwise = shiftL 1 (ceiling (logBase 2 (fromIntegral n :: Double)))
  where
    isPowerOf2 k = k > 0 && popCount k == 1

-- | real-to-complex Fast Fourier Transform
-- | convert from unboxed to storable vector for FFTW, then back to unboxed
fft :: VU.Vector Double -> VU.Vector (Complex Double)
fft vec
  | VU.null vec = VU.empty
  | otherwise =
      let storable = convert vec :: VS.Vector Double
          -- for real input data, dftR2C is more efficient as it leverages symmetry
          result = FFT.run FFT.dftR2C storable
       in convert result

-- | complex-to-real Inverse Fast Fourier Transform
ifft :: VU.Vector (Complex Double) -> VU.Vector Double
ifft vec
  | VU.null vec = VU.empty
  | otherwise =
      let storable = convert vec :: VS.Vector (Complex Double)
          result = FFT.run FFT.dftC2R storable
       in convert result

-- | convolution of two signals using FFT
convolve :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
convolve a b
  | VU.null a || VU.null b = VU.empty
  | VU.length a == 1 = VU.map (* VU.head a) b -- optimization for scalar multiplication
  | VU.length b == 1 = VU.map (* VU.head b) a
  | otherwise =
      let na = VU.length a
          nb = VU.length b
          nc = na + nb - 1
          nextPow2 = nextPowerOf2 nc

          -- Zero-pad to nextPow2 length (power of 2 is more efficient for FFT)
          padded_a = VU.concat [a, VU.replicate (nextPow2 - na) 0]
          padded_b = VU.concat [b, VU.replicate (nextPow2 - nb) 0]

          -- For full convolution, use complex FFT with zero imaginary part
          complex_a = VU.map (:+ 0) padded_a
          complex_b = VU.map (:+ 0) padded_b

          -- Convert to storable vectors for FFTW
          storable_a = convert complex_a :: VS.Vector (Complex Double)
          storable_b = convert complex_b :: VS.Vector (Complex Double)

          -- Forward FFTs
          fft_a = FFT.run FFT.dft storable_a
          fft_b = FFT.run FFT.dft storable_b

          -- Multiply in frequency domain (complex multiplication)
          fft_c = VS.zipWith (*) fft_a fft_b

          -- Inverse FFT
          conv_complex = FFT.run FFT.idft fft_c

          -- Extract real part and take only the needed nc elements
          conv_real = VU.map realPart (convert conv_complex)
       in VU.take nc conv_real

-- | apply a Blackman-Harris window function to a vector
-- | this window function is optimal for precision frequency separation
applyWindow :: VU.Vector Double -> VU.Vector Double
applyWindow vec =
  let n = VU.length vec
      window = VU.generate n $ \i ->
        let x = fromIntegral i / fromIntegral (n - 1)
         in 0.35875
              - 0.48829 * cos (2 * pi * x)
              + 0.14128 * cos (4 * pi * x)
              - 0.01168 * cos (6 * pi * x)
   in VU.zipWith (*) vec window

-- | create a windowed-sinc low-pass filter
createLowPassFilter :: Int -> Double -> Double -> VU.Vector Double
createLowPassFilter size cutoffFreq sampleRate =
  -- Ensure size is odd for symmetric filter with integer group delay
  let size' = if even size then size + 1 else size
      nyquist = sampleRate / 2
      normalizedCutoff = cutoffFreq / nyquist
      mid = size' `div` 2

      -- Create sinc function (impulse response of ideal low-pass filter)
      sinc = VU.generate size' $ \i ->
        let n = fromIntegral i - fromIntegral mid
         in if n == 0
              then normalizedCutoff
              else sin (pi * normalizedCutoff * n) / (pi * n)

      -- Apply Blackman-Harris window to reduce ringing
      windowed = applyWindow sinc

      -- Normalize to ensure unity gain at DC
      filterSum = VU.sum windowed
   in VU.map (/ filterSum) windowed

-- | Create a windowed-sinc high-pass filter using spectral inversion
createHighPassFilter :: Int -> Double -> Double -> VU.Vector Double
createHighPassFilter size cutoffFreq sampleRate =
  let size' = if even size then size + 1 else size
      mid = size' `div` 2
      lpf = createLowPassFilter size' cutoffFreq sampleRate

      -- Spectral inversion: negate all samples and add impulse at center
      hpf = VU.imap (\i x -> if i == mid then 1 - x else -x) lpf
   in hpf

-- | Create a windowed-sinc band-pass filter
createBandPassFilter :: Int -> Double -> Double -> Double -> VU.Vector Double
createBandPassFilter size lowCutoff highCutoff sampleRate =
  let size' = if even size then size + 1 else size
      nyquist = sampleRate / 2
      normLow = lowCutoff / nyquist
      normHigh = highCutoff / nyquist
      mid = size' `div` 2

      -- Create high and low pass responses
      hpSinc = VU.generate size' $ \i ->
        let n = fromIntegral i - fromIntegral mid
         in if n == 0
              then normHigh
              else sin (pi * normHigh * n) / (pi * n)

      lpSinc = VU.generate size' $ \i ->
        let n = fromIntegral i - fromIntegral mid
         in if n == 0
              then normLow
              else sin (pi * normLow * n) / (pi * n)

      -- Bandpass is high-pass minus low-pass
      bpSinc = VU.zipWith (-) hpSinc lpSinc

      -- Apply window function
      bpFilter = applyWindow bpSinc

      -- Normalize for unity gain at center frequency
      filterSum = VU.sum (VU.map abs bpFilter)
   in VU.map (/ filterSum) bpFilter

-- | Quadrature demodulation (I/Q demodulation)
-- | Extracts the I and Q components from an AM signal
quadratureDemodulate :: VU.Vector Double -> Double -> Double -> (VU.Vector Double, VU.Vector Double)
quadratureDemodulate signal carrierFreq sampleRate =
  let timeStep = 1.0 / sampleRate
      n = VU.length signal

      -- Generate local oscillator signals (in-phase and quadrature)
      iCarrier = VU.generate n $ \i ->
        cos (2 * pi * carrierFreq * fromIntegral i * timeStep)
      qCarrier = VU.generate n $ \i ->
        sin (2 * pi * carrierFreq * fromIntegral i * timeStep)

      -- Multiply input signal with oscillator signals
      iMixed = VU.zipWith (*) signal iCarrier
      qMixed = VU.zipWith (*) signal qCarrier

      -- Low-pass filter to remove 2x carrier frequency components
      -- For NTSC, typical bandwidth is 4.2 MHz, carrier is 3.58 MHz
      cutoffFreq = min (carrierFreq / 2) (sampleRate / 4)
      filterSize = 101 -- Should be odd and large enough for good frequency response
      lpFilter = createLowPassFilter filterSize cutoffFreq sampleRate

      -- Apply filter to mixed signals
      iFiltered = convolve iMixed lpFilter
      qFiltered = convolve qMixed lpFilter

      -- Trim filter delay (group delay = filterSize/2)
      filterDelay = filterSize `div` 2
      iDemodulated = VU.take (n - filterDelay) $ VU.drop filterDelay iFiltered
      qDemodulated = VU.take (n - filterDelay) $ VU.drop filterDelay qFiltered
   in (iDemodulated, qDemodulated)
