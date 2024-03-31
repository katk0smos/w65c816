# W65C816
An emulator for Western Design Center's W65C816S, a 16-bit derivative of the
WDC W65C02S, which is itself a derivative of the MOS 6502.

I'm developing this as there doesn't appear to be any existing standalone '816
cores in Rust, and certainly not at the level of emulation I'm doing
(though we can go deeper!). For other programming languages, 816 cores seem
pretty sparse, and as far as I know, none provide a bus-accurate cycle-accurate
emulator with all signals.

## Warning
This emulator is very WIP and is not complete. Many instructions are missing,
and ABORT support is missing for many instructions.

The external API is finalized, and you are free to base code off of this with
the expectation that it will work at some point in the future.

Current plans:
 - [ ] continue adding instructions
 - [ ] continue adding addressing modes
 - [ ] ensure ABORT is implemented for all instructions
 - [ ] tests, tests, and more tests

I am not providing a table showing currently implemented instructions due to
the effort involved. You'll either have to open `src/instructions.rs` and look
at the table there or just run some code and see if it panics. Not a great
solution, I know. At some point all of the instructions will be implemented so
this won't be a problem.
