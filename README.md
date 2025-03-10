# NTSC Demodulator

This repo contains a NTSC demodulator that I made in Haskell for [Travis Whitaker](https://x.com/TravisMWhitaker/status/1873204170070868225).

Apart from Haskell which I've been learning for the past few weeks, I'm pretty much learning all of this stuff from scratch as we go.

## Part 1. Figuring out what is what

#### NTSC (National Television System Committee)
The NTSC created the first American standard for broadcasting analog color television. It defines a way to encode analog video and audio signals onto a radio-frequency (RF) carrier wave in order for to be transmitted via airwaves / cables.

This composite video signal carries multiple types of data:
- Luminance (Y): brightness (of a monochrome image) which contains spatial and intesntiy information
- Chrominance (C): color information encoded as two separate signals called I and Q
- Synchronization (Sync): timing signals which tell the receiver when to start drawing each line (horizontal sync) and frame (vertical sync), respectively
- Audio Signal: transmitted separately within the RF spectrum but still close to the video signal

NTSC composite signal structure:
- 
