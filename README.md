# Chip-8 emulator - Made with Haskell

Complete implementation of the CHIP-8 machine language in Haskell. Uses SDL for graphics

This emulator implements instructions like the HP48 version of the machine instead of the original COSMAC VIP microcomputer.

## Usage
```console
git clone --recurse-submodules https://github.com/JColonnello/chip8
cd chip8
cabal update
cabal build
cabal run chip8 -- [rom.ch8]
```

## ROMs included

- test_opcode.ch8 by corax89 [source](https://github.com/corax89/chip8-test-rom)
- bc_test.ch8 by BestCoder <bestcoder@ymail.com> [mirrored here](https://github.com/daniel5151/AC8E)
- Chip-8 Program Pack by Revival Studios [mirrored here](https://github.com/kripod/chip8-roms)
