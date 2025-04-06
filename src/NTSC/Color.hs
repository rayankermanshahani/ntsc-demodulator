{- src/NTSC/Color.hs -}
module NTSC.Color
  ( extractColorBurst,
    extractLuminance,
    demodulateIQ,
    convertYIQtoRGB,
  )
where

import DSP
import Data.Vector (convert)
import NTSC.Types

extractColorBurst :: LinePartitionedSignal -> ColorReferencedSignal
extractColorBurst = error "TODO"

extractLuminance :: ColorReferencedSignal -> LuminanceExtractedSignal
extractLuminance = error "TODO"

demodulateIQ :: LuminanceExtractedSignal -> ColorReference -> YIQSignal
demodulateIQ = error "TODO"

convertYIQtoRGB :: YIQSignal -> RGBSignal
convertYIQtoRGB = error "TODO"
