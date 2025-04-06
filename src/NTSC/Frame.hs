{- src/NTSC/Frame.hs -}
module NTSC.Frame
  ( assembleFrames,
    assembleOutput,
  )
where

import NTSC.Types

assembleFrames :: RGBSignal -> DecodedVideo
assembleFrames = error "TODO"

assembleOutput :: DecodedVideo -> AudioSamples -> Double -> NTSCDemodulated
assembleOutput = error "TODO"
