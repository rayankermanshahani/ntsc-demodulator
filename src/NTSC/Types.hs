module NTSC.Types
  ( -- * signal types
    RawSignal,
    SignalParams (..),
    NormalizedSignal,
    FilteredSignal,

    -- * synchronization
    SyncInfo (..),
    SyncedSignal,
    ScanLine (..),
    LinePartitionedSignal,

    -- * color processing
    ColorReference (..),
    ColorReferencedSignal,
    LuminanceData,
    LuminanceExtractedSignal,
    ChrominanceData (..),
    YIQSignal,

    -- * output types
    RGBData (..),
    RGBSignal,
    VideoFrame (..),
    DecodedVideo,
    AudioSamples,
    Metadata (..),
    NTSCDemodulated (..),
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- signal types

type RawSignal = VU.Vector Double

data SignalParams = SignalParams
  { sampleRate :: Double,
    centerFreq :: Double
  }

type NormalizedSignal = VU.Vector Double

type FilteredSignal = VU.Vector Double

-- synchronization types

data SyncInfo = SyncInfo
  { hSyncPositions :: VU.Vector Int,
    vSyncPositions :: VU.Vector Int,
    lineStarts :: VU.Vector Int,
    fieldStarts :: VU.Vector Int,
    isOddField :: Bool
  }

type SyncedSignal = (FilteredSignal, SyncInfo)

-- scan line types

data ScanLine = ScanLine
  { lineNumber :: Int,
    fieldNumber :: Int,
    samples :: VU.Vector Double,
    syncPosition :: Int
  }

type LinePartitionedSignal = V.Vector ScanLine

-- color types
data ColorReference = ColorReference
  { frequency :: Double, -- 3.579545 MHz
    phase :: Double, -- reference phase
    amplitude :: Double -- burst ampltitude
  }

type ColorReferencedSignal = (V.Vector ScanLine, ColorReference)

type LuminanceData = VU.Vector Double

type LuminanceExtractedSignal = V.Vector (ScanLine, LuminanceData)

data ChrominanceData = ChrominanceData
  { iValues :: VU.Vector Double,
    qValues :: VU.Vector Double
  }

type YIQSignal = V.Vector (ScanLine, LuminanceData, ChrominanceData)

-- output types

data RGBData = RGBData
  { rValues :: VU.Vector Double,
    gValues :: VU.Vector Double,
    bValues :: VU.Vector Double
  }

type RGBSignal = V.Vector (ScanLine, RGBData)

data VideoFrame = VideoFrame
  { frameNumber :: Int,
    lines :: V.Vector (Int, RGBData),
    width :: Int,
    height :: Int
  }

type DecodedVideo = V.Vector VideoFrame

type AudioSamples = VU.Vector Double

data Metadata = Metdata
  { originalSampleRate :: Double,
    framesProcessed :: Int,
    processingTime :: Double
  }

data NTSCDemodulated = NTSCDemodulated
  { videoFrames :: DecodedVideo,
    audioSamples :: AudioSamples,
    metadata :: Metadata
  }
