# NTSC Demodulator

This repo contains a NTSC demodulator that I made in Haskell for [Travis Whitaker](https://x.com/TravisMWhitaker/status/1873204170070868225).

Apart from Haskell which I've been learning for the past few weeks, I'm pretty much learning all of this stuff from scratch as we go.

## Part 1. What is What

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
