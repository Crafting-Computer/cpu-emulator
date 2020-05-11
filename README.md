# CPU Emulator

Emulate a simple 32-bit CPU containing
* Screen: 512 * 256 * (8 bit color per pixel) or 2 ^ 15 registers each holding 4 pixels
* Ram: 2 ^ 16 registers
* Rom: 2 ^ 16 registers
* Register: 32 bits

# Development
Run [`elm-live`](https://github.com/wking-io/elm-live):
```
elm-live src/CpuEmulator.elm --start-page public/index.html -- --output=elm.js
```

# License
MIT