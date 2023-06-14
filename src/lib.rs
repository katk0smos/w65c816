#![no_std]

/// Trait that systems should implement.
/// Any given function will be called once per cycle, but not all functions
/// will be called every cycle.
pub trait System {
    fn read(&mut self, addr: u32, addr_type: AddressType, signals: &Signals) -> u8;
    fn write(&mut self, addr: u32, data: u8, signals: &Signals) -> ();
    fn irq(&mut self) -> bool { false }
    fn nmi(&mut self) -> bool { false }
    fn res(&mut self) -> bool { false }
    fn rdy(&mut self) -> bool { true }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressType {
    Invalid,
    Data,
    Program,
    Opcode,
    Vector,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Interrupt {
    Reset,
    Nmi,
    Irq,
}

/// CPU Signals
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Signals {
    e: bool,
    mlb: bool,
    mx: bool,
    rdy: bool,
}

impl Default for Signals {
    fn default() -> Self {
        Self {
            e: true,
            mlb: true,
            mx: true,
            rdy: true,
        }
    }
}

impl Signals {
    /// Emulation Status (E)
    ///
    /// The Emulation Status output reflects the state of the Emulation (E) mode
    /// flag in the Processor Status (P) Register. This signal may be thought of
    /// as an opcode extension and used for memory and system management.
    pub fn e(&self) -> bool {
        self.e
    }

    /// Memory Lock (MLB)
    ///
    /// The Memory Lock active low output may be used to ensure the integrity of 
    /// Read Modify Write instructions in a multiprocessor system. Memory Lock
    /// indicates the need to defer arbitration of the next bus cycle.
    /// Memory Lock is low during the last three or five cycles of ASL, DEC, INC,
    /// LSR, ROL, ROR, TRB, and TSB memory referencing instructions, depending on
    /// the state of the M flag.
    pub fn mlb(&self) -> bool {
        self.mlb
    }
    
    /// Memory/Index Select Status (MX)
    ///
    /// The Memory/Index Select Status multiplexed output reflects the state of the Accumulator (M) and Index (X) select flags (bits 5 and 4 of the Processor Status (P)
    /// Register. Flag M is valid during PHI2 negative transition. and Flag X is
    /// valid during PHI2 positive transition. These bits may be thought of as
    /// opcode extensions and may be used for memory and system management.
    pub fn mx(&self) -> bool {
        self.mx
    }

    /// Ready (RDY)
    ///
    /// TODO
    pub fn rdy(&self) -> bool {
        self.rdy
    }
}

/// CPU Status Flags
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Flags {
    pub brk: bool,
    pub carry: bool,
    pub decimal: bool,
    pub emulation: bool,
    pub interrupt_disable: bool,
    pub negative: bool,
    pub overflow: bool,
    pub zero: bool,
    pub mem_sel: bool,
    pub index_sel: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Self {
            brk: false,
            carry: true,
            decimal: false,
            emulation: true,
            interrupt_disable: true,
            negative: true,
            overflow: false,
            zero: true,
            mem_sel: true,
            index_sel: false,
        }
    }
}

impl Flags {
    const CARRY: u8 = 0b00000001;
    const EMU: u8 = 0b00000001;
    const ZERO: u8 = 0b00000010;
    const IRQ_DISABLE: u8 = 0b00000100;
    const DECIMAL: u8 = 0b00001000;
    const INDEX_SEL: u8 = 0b00010000;
    const BRK_BIT: u8 = 0b00010000;
    const MEM_SEL: u8 = 0b00100000;
    const OVERFLOW: u8 = 0b01000000;
    const NEGATIVE: u8 = 0b10000000;
    
    /// Returns the flags as an 8-bit byte, with various options for representation.
    /// 
    /// # Arguments
    /// `xce`: Exchanges the carry bit for the emulation bit if true.
    /// `is_native`: Whether the index select and memory select bits should be
    /// exposed (index replacing brk).
    ///
    /// # Returns
    /// A byte following one of the following patterns:
    /// `xce = false; is_native = false` => `NV1BDIZC`
    /// `xce = true; is_native = false` => `NV1BDIZE`
    /// `xce = false; is_native = true` => `NVMXDIZC`
    /// `xce = true; is_native = true` => `NVMXDIZE`
    pub fn as_byte(self, xce: bool, is_native: bool) -> u8 {
        let mut byte = 0u8;

        if self.zero {
            byte |= Self::ZERO;
        }

        if self.interrupt_disable {
            byte |= Self::ZERO;
        }

        if self.decimal {
            byte |= Self::DECIMAL;
        }

        if self.overflow {
            byte |= Self::OVERFLOW;
        }

        if self.negative {
            byte |= Self::NEGATIVE;
        }

        if xce && self.emulation {
            byte |= Self::EMU;
        } else if !xce && self.carry {
            byte |= Self::CARRY;
        }

        if !is_native || (is_native && self.mem_sel) {
            byte |= Self::MEM_SEL;
        } else if is_native && self.index_sel {
            byte |= Self::INDEX_SEL;
        }

        byte
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Default)]
enum State {
    #[default]
    Fetch,
    Reset,
    Irq,
    Nmi,
    NOP,
}

/// 65c816
#[derive(Clone, Debug)]
pub struct CPU {
    /// Instruction Register
    ir: u8,
    /// Timing Control Unit
    tcu: usize,
    /// Accumulator
    a: u16,
    /// Data Bank Register
    dbr: u8,
    /// Direct
    d: u16,
    /// X Index Register
    x: u16,
    /// Y Index Register
    y: u16,
    /// Program Bank Register
    pbr: u8,
    /// Program Counter
    pc: u16,
    /// Stack Pointer
    s: u16,
    /// RDY signal internal
    rdy: bool,
    /// Interrupt being handled (internal)
    int: Option<Interrupt>,
    /// Wait for interrupt (WAI) status
    wai: bool,
    /// Stop status
    stp: bool,
    /// Flags
    flags: Flags,
    /// Signals
    signals: Signals,
    /// CPU State
    state: State,
}

impl Default for CPU {
    fn default() -> Self {
        Self {
            ir: 0xff,
            tcu: 0xff,
            a: 0xff,
            dbr: 0xff,
            d: 0xffff,
            x: 0xffff,
            y: 0xffff,
            pbr: 0xff,
            pc: 0xffff,
            s: 0xffff,
            rdy: true,
            int: None,
            wai: false,
            stp: false,
            flags: Flags::default(),
            signals: Signals::default(),
            state: State::default(),
        }
    }
}

impl CPU {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn cycle(&mut self, system: &mut impl System) {
        let (rdy, res, nmi, irq) = (
            self.rdy(system),
            system.res(),
            system.nmi(),
            system.irq(),
        );

        if !rdy && !self.wai && !res {
            return;
        }

        if self.stp && !res {
            return;
        }

        self.tcu += 1;

        fn implied(cpu: &mut CPU, system: &mut impl System) {
            cpu.signals.mlb = false;

            let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let _ = system.read(effective, AddressType::Invalid, &cpu.signals);
            cpu.pc = cpu.pc.wrapping_add(1);

            cpu.state = State::Fetch;
        }

        match self.state {
            State::Reset => {
                if res {
                    self.tcu = 0;
                    return;
                }

                match self.tcu {
                    1 | 2 => {
                        self.stp = false;
                        self.wai = false;
                        self.flags.emulation = true;
                        self.flags.mem_sel = true;
                        self.flags.index_sel = true;
                        self.flags.decimal = false;
                        self.flags.interrupt_disable = true;
                        self.signals.mx = true;
                        self.d = 0;
                        self.dbr = 0;
                        self.pbr = 0;
                        ByteRef::High(&mut self.x).set(0x00);
                        ByteRef::High(&mut self.y).set(0x00);

                        let _ = system.read(0x0000ff, AddressType::Data, &self.signals);
                    }
                    3 => {
                        let _ = system.read(0x000100, AddressType::Data, &self.signals);
                        self.s = 0x01ff;
                    },
                    4 | 5 => {
                        let _ = system.read(self.s as u32, AddressType::Data, &self.signals);
                        self.s = self.s.wrapping_sub(1);
                    }
                    6 => {
                        let lo = system.read(0x00fffc, AddressType::Vector, &self.signals);
                        ByteRef::Low(&mut self.pc).set(lo);
                    }
                    7 => {
                        let hi = system.read(0x00fffd, AddressType::Vector, &self.signals);
                        ByteRef::High(&mut self.pc).set(hi);
                        self.state = State::Fetch;
                    }
                    _ => unreachable!(),
                }
            }
            State::Fetch => {
                if res {
                    self.ir = 0x00;
                    self.state = State::Reset;
                    return;
                } else if nmi {
                    self.ir = 0x00;
                    self.state = State::Nmi;
                    return;
                } else if irq {
                    self.ir = 0x00;
                    self.state = State::Irq;
                    return;
                }

                self.signals.mlb = false;
                self.tcu = 0;

                let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                self.ir = system.read(effective, AddressType::Opcode, &self.signals);
                self.pc = self.pc.wrapping_add(1);
        
                match self.ir {
                    0xEA => {
                        self.state = State::NOP;
                        self.tcu = 0;
                    }
                    _ => todo!(),
                }
            }
            State::NOP => implied(self, system),
            _ => todo!(),
        }
    }

    /// RDY signal, with internal pulldown.
    pub fn rdy(&self, system: &mut impl System) -> bool {
        system.rdy() & self.rdy
    }

    /// Signals
    pub fn signals(&self) -> &Signals {
        &self.signals
    }

    /// X Index Register (Low)
    fn xl(&self) -> u8 {
        let mut x = self.x;
        ByteRef::Low(&mut x).get()
    }
    
    /// X Index Register (Low)
    fn xl_mut(&mut self) -> ByteRef {
        ByteRef::Low(&mut self.x)
    }
    
    /// X Index Register (High)
    fn xh(&self) -> u8 {
        let mut x = self.x;
        ByteRef::High(&mut x).get()
    }

    /// X Index Register (High)
    fn xh_mut(&mut self) -> ByteRef {
        ByteRef::High(&mut self.x)
    }

    /// Y Index Register (Low)
    fn yl(&self) -> u8 {
        let mut y = self.y;
        ByteRef::Low(&mut y).get()
    }
    
    /// Y Index Register (Low)
    fn yl_mut(&mut self) -> ByteRef {
        ByteRef::Low(&mut self.y)
    }
    
    /// Y Index Register (High)
    fn yh(&self) -> u8 {
        let mut y = self.y;
        ByteRef::High(&mut y).get()
    }

    /// Y Index Register (High)
    fn yh_mut(&mut self) -> ByteRef {
        ByteRef::High(&mut self.y)
    }

    /// Read next program byte
    fn next_prg_byte(&mut self, system: &mut impl System, inc_bank: bool) -> u8 {
        let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
        let (pc, overflow) = self.pc.overflowing_add(1);
        self.pc = pc;
        
        if inc_bank && overflow {
            self.pbr = self.pbr.wrapping_add(1);
        }

        system.read(effective, AddressType::Program, &self.signals)
    }

    fn absolute_a(&self, system: &mut impl System, addr: u16) -> u32 {
        ((self.dbr as u32) << 16) | addr as u32
    }
}

/// A reference to a specific byte in a word
#[derive(Debug, PartialEq, Eq)]
pub enum ByteRef<'a> {
    Low(&'a mut u16),
    High(&'a mut u16),
}

impl ByteRef<'_> {
    /// Gets the byte
    pub fn get(&self) -> u8 {
        match self {
            ByteRef::Low(x) => (**x & 0xff) as u8,
            ByteRef::High(x) => (**x >> 8) as u8,
        }
    }

    /// Sets the byte
    pub fn set(&mut self, value: u8) {
        match self {
            ByteRef::Low(x) => **x = (**x & 0xff00) | (value as u16),
            ByteRef::High(x) => **x = (**x & 0xff) | ((value as u16) << 8),
        }
    }

    /// Swap to the other byte
    pub fn swap(self) -> Self {
        match self {
            ByteRef::Low(x) => ByteRef::High(x),
            ByteRef::High(x) => ByteRef::Low(x),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
