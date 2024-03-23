# W65C816
An emulator for Western Design Center's W65C816S, a 16-bit derivative of the
WDC W65C02S, which is itself a derivative of the MOS 6502.

## Warning
This emulator is very WIP and is not complete. Many instructions are missing,
and ABORT support is spotty. Instruction implementations are somewhat tacked on,
and maintaining will be pretty much impossible. For these reasons I'll be performing
a full rewrite. This repo and crate exists to preserve my philosophy, good and bad,
so that I can ensure the final code isn't spaghetti.

The external API is finalized, and you are free to base code off of this with
the expectation that it will work at some point in the future.

Current plans:
 - [ ] rewrite

Current plan for the rewrite is to implement each addressing mode as a function
rather than re-implementing it over-and-over in each instruction. I'd like to
move the instruction implementations out of the main update function as well. I
think sacrificing a small amount of memory to move ABORT support outside
instructions would be worth it as well, just store register values during a
fetch cycle and then restore if an ABORT occurs
