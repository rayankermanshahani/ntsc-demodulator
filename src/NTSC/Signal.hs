{- src/NTSC/Signal.hs -}
module NTSC.Signal
  ( readSignal,
    normalizeSignal,
    bandpassFilter,
  )
where

import DSP
import qualified Data.Vector.Unboxed as VU
import qualified NTSC.Types
import qualified NTSC.Types as Types

readSignal :: FilePath -> IO Types.RawSignal
readSignal = error "TODO"

normalizeSignal :: Types.RawSignal -> Types.SignalParams -> Types.NormalizedSignal
normalizeSignal = error "TODO"

bandpassFilter :: Types.NormalizedSignal -> Double -> Double -> Types.FilteredSignal
bandpassFilter = error "TODO"