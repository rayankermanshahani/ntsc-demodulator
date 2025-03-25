module NTSC.Color
  ( extractColorBurst,
    extractLuminance,
    demodulateIQ,
    convertYIQtoRGB,
  )
where

import DSP
import NTSC.Types

extractColorBurst :: LinePartitionedSignal -> ColorReferencedSignal
extractLuminance :: ColorReferencedSignal -> LuminanceExtractedSignal
demodulateIQ :: LuminanceExtractedSignal -> ColorReference -> YIQSignal
convertYIQtoRGB :: YIQSignal -> RGBSignal
