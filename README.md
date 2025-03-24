# NTSC Demodulator

This repo contains a NTSC demodulator that I made in Haskell for [Travis Whitaker](https://x.com/TravisMWhitaker/status/1873204170070868225).

Apart from Haskell which I've been learning for the past few weeks, I'm pretty much learning all of the requisite signal processing as we go. So far I'm enjoying Haskell as it feels like a very natural way to reason about software. This is because, ultimately, I view programming as the process of taking (input) data of one form and transforming it into another form (desirted output) via a series / composition of functions. This means that I need to clearly understand all the possible "shapes" of the data (types / structures) that I will be working with before doing anything else. Once these "shapes" are established, I can then dive into the details for their respective transformations with full confidence. Whenever a specfic function's implementation gets too involved, I can always break it down into simpler tasks until the intermediate shapes feel intuitive again. 

## Shapes and Transformations
### 1. Signal Acquisition
#### Input: 
```haskell
type RawSignal = Vector Double  -- sampled RF values at uniform intervals
data SignalParams = SignalParams {
  sampleRate :: Double,         -- samples per second
  centerFreq :: Double          -- carrier frequency
}
```

#### Operations:
- read continuous RF samples from file or device
- normalize amplitude

#### Output:
```haskell
type NormalizedSignal = Vector Double  -- scaled to standard range
```

### 2. Bandpass Filtering
#### Inputs:
```haskell
type NormalizedSignal = Vector Double
```
#### Operations:
- apply bandpass filter to isolate 6 MHz NTSC channel
- implement vestigial sideband processing
- remove out-of-band noise

#### Output:
```haskell
type FilteredSignal = Vector Double
```

### 3. Synchronization Detection
#### Input:
```haskell
type FilteredSignal = Vector Double
```
#### Operations:
- detect horizontal sync pulses (falling to blanking level)
- identify vertical sync patterns
- locate equalizing pulses
- determine field type (odd/even)

#### Output:
```haskell
data SyncInfo = SyncInfo {
  hSyncPositions :: Vector Int,  -- sample indices of H-sync starts
  vSyncPositions :: Vector Int,  -- sample indices of V-sync starts
  lineStarts     :: Vector Int,  -- start of active video for each line
  fieldStarts    :: Vector Int,  -- start of each field
  isOddField     :: Bool         -- field parity
}

type SyncedSignal = (FilteredSignal, SyncInfo)
```
### 4. Scan Line Partitioning
#### Input: 
``` haskell
type SyncedSignal = (FilteredSignal, SyncInfo)
```
#### Operations:
- divide signal into individual scan lines
- extract active video portion of each line
- group lines into fields

#### Output: 
``` haskell
data ScanLine = ScanLine {
  lineNumber   :: Int,
  fieldNumber  :: Int,
  samples      :: Vector Double,
  syncPosition :: Int
}

type LinePartitionedSignal = Vector ScanLine
```

### 5. Color Burt Extraction and Analysis
#### Input:
```haskell
type LinePartitionedSignal = Vector ScanLine
```
#### Operations:
- extract 8-11 cycles of 3.58 MHz reference burst after H-sync
- calculate phase reference for color demodulation
- measure burst amplitude for calibration

#### Output:
```haskell
data ColorReference = ColorReference {
  frequency  :: Double,  -- should be 3.579545 MHz
  phase      :: Double,  -- reference phase from bursts
  ampltitude :: Double   -- burst amplitude
}

type ColorReferencedSignal = (Vector ScanLine, ColorReference)
```

### 6. Luminance (Y) Extraction
#### Input:
```haskell
type ColorReferencedSignal = (Vector ScanLine, ColorReference)
```
#### Operations:
- apply lowpass filter (0-3 MHz) to extract luminance
- scale according to NTSC levels (blanking at 75%, reference white at 15%)
- apply proper gamma correction

#### Output:
```haskell
data LuminanceData = Vector Double  -- Y values for each sample

type LuminanceExtractedSignal = Vector (ScanLine, LuminanceData)
```

### 7. Chrominance Demodulation (I/Q Extraction)
#### Input:
```haskell
type LuminanceExtractedSignal = Vector (ScanLine, LuminanceData)
data ColorReference = ColorReference { ... }
```
#### Operations:
- use color burst as phase reference
- demodulate I component (in-phase, wide-band)
- demodulate Q component (quadrature, narrow-band)
- apply appropriate bandpass filtering to each component
    - I-channel: up to 1.3 MHz
    - Q-channel: up to 0.5 MHz

#### Output:
```haskell
data ChrominanceData = ChrominanceData {
  iValues :: Vector Double,  -- I component values
  qValues :: Vector Double   -- Q component values
}

type YIQSignal = Vector (ScanLine, LuminanceData, ChrominanceData)
```

### 8. YIQ to RGB Conversion
#### Input:
```haskell
type YIQSignal = Vector (ScanLine, LuminanceData, ChrominanceData)
```
#### Operations:
- convert each YIQ triplet to RGB using NTSC matrix
- apply gamma correction for display

#### Output:
```haskell
data RGBData = RGBData {
  rValues :: Vector Double,
  gValues :: Vector Double,
  bValues :: Vector Double
}

type RGBSignal = Vector (ScanLine, RGBData)
```

### 9. Frame Assembly
#### Input:
```haskell
type RGBSignal = Vector (ScanLine, RGBData)
```
#### Operations:
- order scan lines by field and line number
- handle interlacing (alternating odd/even fields)
- create complete frames

#### Output:
```haskell
data VideoFrame = VideoFrame {
  frameNumber :: Int,
  lines       :: Vector (Int, RGBData),  -- line number and RGB data
  width       :: Int, 
  height      :: Int
}

type DecodedVideo = Vector VideoFrame
```

### 10. Audio Processing
#### Input:
```haskell
type FilteredSignal = Vector Double  -- original filtered signal
```
#### Operations:
- apply bandpass filter around 4.5 MHz (audio carrier)
- FM demodulate the audio carrier
- apply 75 μsec de-emphasis filter

#### Output:
```haskell
type AudioSamples = Vector Double  -- audio sampels values
```

### 11. Final Output Assembly
#### Input:
```haskell
type DecodedVideo = Vector VideoFrame
type AudioSamples = Vector Double
```
#### Operations:
- synchronize audio with video frames
- format according to output requirements

#### Output:
```haskell
data NTSCDemodulated = NTSCDemodulated {
  videoFrames  :: Vector VideoFrame,
  audioSamples :: AudioSamples,
  metaData     :: MetaData
}
```

## Appendix. Some Good Reference Material

#### NTSC (National Television System Committee)
The [NTSC](https://antiqueradio.org/art/NTSC%20Signal%20Specifications.pdf) created the first American standard for broadcasting analog color television. It defines a way to encode analog video and audio signals onto a radio-frequency (RF) carrier wave in order for to be transmitted via airwaves / cables.

#### Modulation

The process of varying the properties of a carrier signal with a separate [modulation](https://en.wikipedia.org/wiki/Modulation) signal in order to encode and subsequently transmit the modulation signal's information. 

The carrier signal is usually a higher frequency sine wave. Higher frequencies allow the signal to be trasmitted over longer distances while being more resistance to noise, and with respect to practical physical considerations: have reasonably sized antenna (wavelength is inversely proportional to frequency). Sine waves have clean mathematical and physical properties which make designing, analysing, and implementing these systems much easier.

Demodulation is just the reverse process of modulation. We are essentially desconstructing the modulated carrier signal once it is received in order to extract the original low-frequency information signal.

Common types of analog signal modulation:
- Amplitude Modulation ([AM](https://en.wikipedia.org/wiki/Amplitude_modulation)): encoding information in the carrier wave's amplitude (signal strength)
- Frequency Modulation ([FM](https://en.wikipedia.org/wiki/Frequency_modulation)): encoding information in the carrier wave's frequency
- Phase Modulation ([PM](https://en.wikipedia.org/wiki/Phase_modulation)): encoding information in the phase (timing) of the carrier wave
- Quadrature Ampltidude Modulation ([QAM](https://en.wikipedia.org/wiki/Quadrature_amplitude_modulation))
- Vestigial Sideband Modulation ([VSB](https://en.wikipedia.org/wiki/Vestigial_sideband_modulation))
