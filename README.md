# NTSC Demodulator

This repo contains a NTSC demodulator that I made in Haskell for [Travis Whitaker](https://x.com/TravisMWhitaker/status/1873204170070868225).

Apart from Haskell which I've been learning for the past few weeks, I'm pretty much learning all of the requisite signal processing as we go. So far I'm enjoying Haskell as it feels like a very natural way to reason about software. This is because, ultimately, I view programming as the process of taking (input) data of one form and transforming it into another form (desirted output) via a series / composition of functions. This means that I need to clearly understand all the possible "shapes" of the data (types / structures) that I will be working with before doing anything else. Once these "shapes" are established, I can then dive into the details for their respective transformations with full confidence. Whenever a specfic function's implementation gets too involved, I can always break it down into simpler tasks until the intermediate shapes feel intuitive again. 

## Transformations
### 1. Signal Acquisition
#### Operations:
- read continuous RF samples from file or device
- normalize amplitude

### 2. Bandpass Filtering
#### Operations:
- apply bandpass filter to isolate 6 MHz NTSC channel
- implement vestigial sideband processing
- remove out-of-band noise

### 3. Synchronization Detection
#### Operations:
- detect horizontal sync pulses (falling to blanking level)
- identify vertical sync patterns
- locate equalizing pulses
- determine field type (odd/even)

### 4. Scan Line Partitioning
#### Operations:
- divide signal into individual scan lines
- extract active video portion of each line
- group lines into fields

### 5. Color Burt Extraction and Analysis
#### Operations:
- extract 8-11 cycles of 3.58 MHz reference burst after H-sync
- calculate phase reference for color demodulation
- measure burst amplitude for calibration

### 6. Luminance (Y) Extraction
#### Operations:
- apply lowpass filter (0-3 MHz) to extract luminance
- scale according to NTSC levels (blanking at 75%, reference white at 15%)
- apply proper gamma correction

### 7. Chrominance Demodulation (I/Q Extraction)
#### Operations:
- use color burst as phase reference
- demodulate I component (in-phase, wide-band)
- demodulate Q component (quadrature, narrow-band)
- apply appropriate bandpass filtering to each component
    - I-channel: up to 1.3 MHz
    - Q-channel: up to 0.5 MHz

### 8. YIQ to RGB Conversion
#### Operations:
- convert each YIQ triplet to RGB using NTSC matrix
- apply gamma correction for display

### 9. Frame Assembly
#### Operations:
- order scan lines by field and line number
- handle interlacing (alternating odd/even fields)
- create complete frames

### 10. Audio Processing
#### Operations:
- apply bandpass filter around 4.5 MHz (audio carrier)
- FM demodulate the audio carrier
- apply 75 μsec de-emphasis filter

### 11. Final Output Assembly
#### Operations:
- synchronize audio with video frames
- format according to output requirements

## Appendix. Some Reference Information on the NTSC and General Signal Processing (WIP)

#### NTSC (National Television System Committee)
The NTSC created the first American standard for broadcasting analog color television. It defines a way to encode analog video and audio signals onto a radio-frequency (RF) carrier wave in order for to be transmitted via airwaves / cables.

This broadcast signal carries multiple types of data:
- Luminance (Y): brightness (of a monochrome image) which contains spatial and intesntiy information
- Chrominance (C): color information encoded as two separate signals called I and Q
- Synchronization (Sync): timing signals which tell the receiver when to start drawing each line (horizontal sync) and frame (vertical sync), respectively
- Audio Signal: transmitted separately within the RF spectrum but still close to the video signal

These signals are combined into a single composite video signal.

#### Modulation

The process of varying the properties of a carrier signal with a separate modulation signal in order to encode and subsequently transmit the modulation signal's information. 

The carrier signal is usually a higher frequency sine wave. Higher frequencies allow the signal to be trasmitted over longer distances while being more resistance to noise, and with respect to practical physical considerations: have reasonably sized antenna (wavelength is inversely proportional to frequency). Sine waves have clean mathematical and physical properties which make designing, analysing, and implementing these systems much easier.

Demodulation is just the reverse process of modulation. We are essentially desconstructing the modulated carrier signal once it is received in order to extract the original low-frequency information signal.

The three common types of analog signal modulation are:
1. Amplitude Modulation (AM): encoding information in the carrier wave's amplitude (signal strength)
2. Frequency Modulation (FM): encoding information in the carrier wave's frequency
3. Phase Modulation (PM): encoding information in the phase (timing) of the carrier wave
