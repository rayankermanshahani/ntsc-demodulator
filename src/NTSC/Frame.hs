module NTSC.Frame
  ( assembleFrames,
    assembleOutput,
  )
where

import NTSC.Types

assembleFrames :: RGBSignal -> DecodedVideo
assembleOutput :: DecodedVideo -> AudioSamples -> Double -> NTSCDemodulated
