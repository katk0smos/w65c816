# W65C816
An emulator for Western Design Center's W65C816S, a 16-bit derivative of the
WDC W65C02S, which is itself a derivative of the MOS 6502.

I'm developing this as there doesn't appear to be any existing standalone '816
cores in Rust, and certainly not at the level of emulation I'm doing
(though we can go deeper!). For other programming languages, 816 cores seem
pretty sparse, and as far as I know, none provide a bus-accurate cycle-accurate
emulator with all signals.

## Warning
While all instructions are implemented, correct behavior has not been verified
for many instructions and addressing modes.

The external API is finalized, and you are free to base code off of this with
the expectation that it will work at some point in the future.

Current plans:
- [x] continue adding instructions
- [x] continue adding addressing modes
- [x] ensure ABORT is implemented for all instructions
  - [ ] ensure ABORT is implemented *correctly* (check with a real '816)
- [ ] tests, tests, and more tests
  - [ ] ideally generate tests from a real '816.
