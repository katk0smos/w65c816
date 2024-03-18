# W65C816
An emulator for Western Design Center's W65C816S, a 16-bit derivative of the
WDC W65C02S, which is itself a derivative of the MOS 6502.

This emulator is very WIP and is not complete. Many instructions are missing,
and ABORT support is spotty. The external API is finalized, however.

Current plans:
 - [ ] abstract all addressing modes out into functions, reducing duplicated code significantly.
 - [ ] finish implementing all of the instructions
 - [ ] ensure ABORT works on all instructions
 - [ ] tests for all instructions
