module NTSC.Signal
  ( readSignal,
    normalizedSignal,
    bandpassFilter,
  )
where

import DSP
import qualified Data.Vector.Unboxed as VU
import qualified NTSC.Types

readSignal :: FilePath -> IO RawSignal
normalizeSignal :: RawSignal -> SignalParams -> NormalizedSignal
bandpassFilter :: NormalizedSignal -> Double -> Double -> FilteredSignal
