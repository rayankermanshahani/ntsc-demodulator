module NTSC.Audio (extractAudio) where

import DSP
import NTSC.Types

extractAudio :: FilteredSignal -> Double -> AudioSamples
