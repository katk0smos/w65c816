#![cfg_attr(not(test), no_std)]

#[cfg(test)]
mod tests;

mod util;
use util::*;

/// Trait that systems should implement.
/// Any given function will be called once per cycle, but not all functions
/// will be called every cycle.
pub trait System {
    fn read(&mut self, addr: u32, addr_type: AddressType, signals: &Signals) -> u8;
    fn write(&mut self, addr: u32, data: u8, signals: &Signals);
    fn irq(&mut self) -> bool {
        false
    }
    fn nmi(&mut self) -> bool {
        false
    }
    fn res(&mut self) -> bool {
        false
    }
    fn rdy(&mut self) -> bool {
        true
    }

    /// Abort
    ///
    /// The Abort pulse active input is used to abort instructions (usually due to an Address Bus condition). A return value of true will inhibit modification of any internal register during the current instruction. Upon completion of this instruction, an interrupt sequence is initiated. The location of the aborted opcode is stored as the return address in stack memory. The Abort vector address is 00FFF8,9 (Emulation mode) or 00FFE8,9 (Native mode). Note that ABORT is a pulse sensitive signal; i.e., an abort will occur whenever true is returned.
    fn abort(&mut self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressType {
    Invalid,
    Data,
    Program,
    Opcode,
    Vector,
}

impl AddressType {
    pub fn into_vda_vpa(&self) -> (bool, bool) {
        match self {
            AddressType::Invalid => (false, false),
            AddressType::Data => (true, false),
            AddressType::Program => (false, true),
            AddressType::Opcode => (true, true),
            AddressType::Vector => (false, false),
        }
    }

    pub fn into_vda(&self) -> bool {
        self.into_vda_vpa().0
    }

    pub fn into_vpa(&self) -> bool {
        self.into_vda_vpa().1
    }

    pub fn into_vpb(&self) -> bool {
        !matches!(self, AddressType::Vector)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Interrupt {
    Reset,
    Nmi,
    Irq,
}

/// CPU Register
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    A,
    X,
    Y,
}

/// CPU Signals
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Signals {
    e: bool,
    mlb: bool,
    m: bool,
    x: bool,
    rdy: bool,
}

impl Default for Signals {
    fn default() -> Self {
        Self {
            e: true,
            mlb: true,
            m: true,
            x: true,
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
    pub fn mx(&self, phi2: bool) -> bool {
        if !phi2 {
            self.m
        } else {
            self.x
        }
    }

    pub fn m(&self) -> bool {
        self.m
    }

    pub fn x(&self) -> bool {
        self.x
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

    /// Returns the flags as an 8-bit byte
    pub fn as_byte(self) -> u8 {
        let is_native = !self.emulation;
        let mut byte = 0u8;

        if self.zero {
            byte |= Self::ZERO;
        }

        if self.interrupt_disable {
            byte |= Self::IRQ_DISABLE;
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

        if self.carry {
            byte |= Self::CARRY;
        }

        if !is_native || self.mem_sel {
            byte |= Self::MEM_SEL;
        }

        if !is_native && self.brk {
            byte |= Self::BRK_BIT;
        } else if is_native && self.index_sel {
            byte |= Self::INDEX_SEL;
        }

        byte
    }

    pub fn set_mask(&mut self, mask: u8, set: bool) {
        if mask & 1 != 0 {
            self.carry = set;
        }

        if mask & 2 != 0 {
            self.zero = set;
        }

        if mask & 4 != 0 {
            self.interrupt_disable = set;
        }

        if mask & 8 != 0 {
            self.decimal = set;
        }

        if self.emulation && mask & 16 != 0 {
            self.brk = set;
        }

        if mask & 0x80 != 0 {
            self.negative = set;
        }

        if mask & 0x40 != 0 {
            self.overflow = set;
        }

        if !self.emulation {
            if mask & 0x20 != 0 {
                self.mem_sel = set;
            }

            if mask & 0x10 != 0 {
                self.index_sel = set;
            }
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Default)]
enum AddressingMode {
    #[default]
    Implied,
    Immediate,
    Absolute,
    Direct,
}

impl AddressingMode {
    pub fn read(self, cpu: &mut CPU, system: &mut impl System) -> Option<TaggedByte> {
        match self {
            AddressingMode::Immediate => match (cpu.flags.emulation, cpu.tcu) {
                (true, 1) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let value = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 1) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let value = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 2) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let value = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    Some(TaggedByte::Data(Byte::High(value)))
                }
                _ => None,
            },
            AddressingMode::Implied => {
                let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                Some(TaggedByte::Data(Byte::Low(system.read(
                    effective,
                    AddressType::Invalid,
                    &cpu.signals,
                ))))
            }
            AddressingMode::Absolute => match (cpu.flags.emulation, cpu.tcu) {
                (_, 1) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let value = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    Some(TaggedByte::Address(Byte::Low(value)))
                }
                (_, 2) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let value = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    Some(TaggedByte::Address(Byte::High(value)))
                }
                (_, 3) => {
                    let effective = ((cpu.dbr as u32) << 16) | (cpu.temp as u32);
                    let value = system.read(effective, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 4) => {
                    let effective = (((cpu.dbr as u32) << 16) | (cpu.temp as u32)).wrapping_add(1);
                    let value = system.read(effective, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::High(value)))
                }
                _ => None,
            },
            AddressingMode::Direct => match (
                cpu.flags.emulation,
                cpu.tcu,
                ByteRef::Low(&mut cpu.d).get() == 0,
            ) {
                (_, 1, _) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let offset = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);

                    Some(TaggedByte::Address(Byte::Low(offset)))
                }
                (_, 2, false) if cpu.d != 0 => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc.wrapping_sub(1) as u32);
                    system.read(effective, AddressType::Invalid, &cpu.signals);
                    None
                }
                (_, 2, true) | (_, 3, false) => {
                    let addr = cpu.d.wrapping_add(ByteRef::Low(&mut cpu.temp).get() as u16) as u32;
                    let value = system.read(addr, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 3, true) | (false, 4, false) => {
                    let addr = cpu.d.wrapping_add(ByteRef::Low(&mut cpu.temp).get() as u16) as u32;
                    let value = system.read(addr, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::High(value)))
                }
                _ => None,
            },
            _ => todo!(),
        }
    }

    pub fn write(
        self,
        cpu: &mut CPU,
        system: &mut impl System,
        mut value: u16,
    ) -> Option<TaggedByte> {
        match self {
            AddressingMode::Absolute => match (cpu.flags.emulation, cpu.tcu) {
                (_, 1) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let value = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    Some(TaggedByte::Address(Byte::Low(value)))
                }
                (_, 2) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let value = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    Some(TaggedByte::Address(Byte::High(value)))
                }
                (_, 3) => {
                    let effective = ((cpu.dbr as u32) << 16) | (cpu.temp as u32);
                    let value = ByteRef::Low(&mut value).get();
                    system.write(effective, value, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 4) => {
                    let effective = (((cpu.dbr as u32) << 16) | (cpu.temp as u32)).wrapping_add(1);
                    let value = ByteRef::High(&mut value).get();
                    system.write(effective, value, &cpu.signals);
                    Some(TaggedByte::Data(Byte::High(value)))
                }
                _ => None,
            },
            AddressingMode::Direct => match (
                cpu.flags.emulation,
                cpu.tcu,
                ByteRef::Low(&mut cpu.d).get() == 0,
            ) {
                (_, 1, _) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let offset = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);

                    Some(TaggedByte::Address(Byte::Low(offset)))
                }
                (_, 2, false) => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc.wrapping_sub(1) as u32);
                    system.read(effective, AddressType::Invalid, &cpu.signals);
                    None
                }
                (_, 2, true) | (_, 3, false) => {
                    let addr = cpu.d.wrapping_add(ByteRef::Low(&mut cpu.temp).get() as u16) as u32;
                    let value = ByteRef::Low(&mut value).get();
                    system.write(addr, value, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 3, true) | (false, 4, false) => {
                    let addr = cpu.d.wrapping_add(ByteRef::Low(&mut cpu.temp).get() as u16) as u32;
                    let value = ByteRef::High(&mut value).get();
                    system.write(addr, value, &cpu.signals);
                    Some(TaggedByte::Data(Byte::High(value)))
                }
                _ => None,
            },
            AddressingMode::Implied | AddressingMode::Immediate => None,
            _ => todo!(),
        }
    }
}

/// Internal state machine
#[derive(Clone, Debug, Copy, PartialEq, Default)]
enum State {
    #[default]
    Fetch,
    Reset,
    Irq,
    Nmi,
    Abort,
    Ld(Register, AddressingMode),
    St(Register, AddressingMode),
    Carry(bool),
    Sep(bool),
    Nop,
    Xce,
    Xba,
    Tcd,
    Tcs,
    Tdc,
    Tsc,
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
    /// Whether to inhibit internal register changes
    aborted: bool,
    /// Temporary register for internal use.
    /// Used for addressing instructions
    temp: u16,
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
            wai: false,
            stp: true,
            aborted: false,
            flags: Flags::default(),
            signals: Signals::default(),
            state: State::default(),
            temp: 0,
        }
    }
}

impl CPU {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn cycle(&mut self, system: &mut impl System) {
        let (rdy, res, nmi, irq, abort) = (
            self.rdy(system),
            system.res(),
            system.nmi(),
            system.irq(),
            system.abort(),
        );

        if !rdy && !self.wai && !res {
            return;
        }

        if self.stp && !res {
            return;
        }

        // Hijack the state for ABORTs
        if self.aborted && self.state == State::Fetch {
            self.state = State::Abort;
        } else if abort {
            self.aborted = true;
        }

        self.tcu += 1;
        self.signals.rdy = rdy;

        fn implied(cpu: &mut CPU, system: &mut impl System) {
            cpu.signals.mlb = false;
            let _ = AddressingMode::Implied.read(cpu, system);
            cpu.state = State::Fetch;
        }

        fn interrupt(
            cpu: &mut CPU,
            system: &mut impl System,
            vector: u16,
            store_pc_p: bool,
            set_b: bool,
        ) {
            match cpu.tcu {
                3 => {
                    if store_pc_p {
                        system.write(
                            (cpu.dbr as u32) << 16 | (cpu.s as u32),
                            ByteRef::High(&mut cpu.pc).get(),
                            &cpu.signals,
                        );
                    } else {
                        system.read(
                            (cpu.dbr as u32) << 16 | (cpu.s as u32),
                            AddressType::Data,
                            &cpu.signals,
                        );
                    }

                    cpu.s = cpu.s.wrapping_sub(1);
                }
                4 => {
                    if store_pc_p {
                        system.write(
                            (cpu.dbr as u32) << 16 | (cpu.s as u32),
                            ByteRef::Low(&mut cpu.pc).get(),
                            &cpu.signals,
                        );
                    } else {
                        system.read(
                            (cpu.dbr as u32) << 16 | (cpu.s as u32),
                            AddressType::Data,
                            &cpu.signals,
                        );
                    }

                    cpu.s = cpu.s.wrapping_sub(1);
                }
                5 => {
                    cpu.flags.brk = set_b;

                    if store_pc_p {
                        system.write(
                            (cpu.dbr as u32) << 16 | (cpu.s as u32),
                            cpu.flags.as_byte(),
                            &cpu.signals,
                        );
                    } else {
                        system.read(
                            (cpu.dbr as u32) << 16 | (cpu.s as u32),
                            AddressType::Data,
                            &cpu.signals,
                        );
                    }

                    cpu.s = cpu.s.wrapping_sub(1);
                }
                6 => {
                    cpu.flags.interrupt_disable = true;
                    ByteRef::Low(&mut cpu.pc).set(system.read(
                        vector as u32,
                        AddressType::Vector,
                        &cpu.signals,
                    ));
                }
                7 => {
                    ByteRef::High(&mut cpu.pc).set(system.read(
                        vector.wrapping_add(1) as u32,
                        AddressType::Vector,
                        &cpu.signals,
                    ));
                    cpu.state = State::Fetch;
                }
                _ => unreachable!(),
            }
        }

        match self.state {
            State::Reset => {
                if res {
                    self.tcu = 0;
                    return;
                }

                match self.tcu {
                    0 | 1 | 2 => {
                        self.stp = false;
                        self.wai = false;
                        self.flags.emulation = true;
                        self.flags.mem_sel = true;
                        self.flags.index_sel = true;
                        self.flags.decimal = false;
                        self.signals.m = true;
                        self.signals.x = true;
                        self.aborted = false;
                        self.d = 0;
                        self.dbr = 0;
                        self.pbr = 0;
                        self.s = 0x0100;
                        ByteRef::High(&mut self.x).set(0x00);
                        ByteRef::High(&mut self.y).set(0x00);

                        let _ = system.read(0x0000ff, AddressType::Invalid, &self.signals);
                    }
                    4 => {
                        self.s = 0x01ff;
                        interrupt(self, system, 0xfffc, false, false);
                    }
                    _ => interrupt(self, system, 0xfffc, false, false),
                }
            }
            State::Nmi => {
                if self.flags.emulation {
                    interrupt(self, system, 0x00fffa, true, false);
                } else {
                    interrupt(self, system, 0x00ffea, true, false);
                }
            }
            State::Irq => {
                if self.flags.emulation {
                    interrupt(self, system, 0x00fffe, true, false);
                } else {
                    interrupt(self, system, 0x00ffee, true, false);
                }
            }
            State::Abort => {
                if self.flags.emulation {
                    interrupt(self, system, 0x00fff8, true, false);
                } else {
                    interrupt(self, system, 0x00ffe8, true, false);
                }
            }
            State::Fetch => {
                self.signals.mlb = false;
                self.tcu = 0;

                if res {
                    self.ir = 0x00;
                    self.stp = false;
                    self.wai = false;
                    self.state = State::Reset;
                    return;
                } else if nmi {
                    self.ir = 0x00;
                    self.wai = false;
                    self.state = State::Nmi;
                    return;
                } else if irq {
                    self.ir = 0x00;
                    self.wai = false;
                    self.state = State::Irq;
                    return;
                }

                let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                self.ir = system.read(effective, AddressType::Opcode, &self.signals);
                self.pc = self.pc.wrapping_add(1);
                self.tcu = 0;

                self.state = match self.ir {
                    0x18 => State::Carry(false),
                    0x1B => State::Tcs,
                    0x38 => State::Carry(true),
                    0x3B => State::Tsc,
                    0x5B => State::Tcd,
                    0x7B => State::Tdc,
                    0x84 => State::St(Register::Y, AddressingMode::Direct),
                    0x85 => State::St(Register::A, AddressingMode::Direct),
                    0x86 => State::St(Register::X, AddressingMode::Direct),
                    0x8C => State::St(Register::Y, AddressingMode::Absolute),
                    0x8D => State::St(Register::A, AddressingMode::Absolute),
                    0x8E => State::St(Register::X, AddressingMode::Absolute),
                    0xA0 => State::Ld(Register::Y, AddressingMode::Immediate),
                    0xA2 => State::Ld(Register::X, AddressingMode::Immediate),
                    0xA4 => State::Ld(Register::Y, AddressingMode::Direct),
                    0xA5 => State::Ld(Register::A, AddressingMode::Direct),
                    0xA6 => State::Ld(Register::X, AddressingMode::Direct),
                    0xA9 => State::Ld(Register::A, AddressingMode::Immediate),
                    0xAC => State::Ld(Register::Y, AddressingMode::Absolute),
                    0xAD => State::Ld(Register::A, AddressingMode::Absolute),
                    0xAE => State::Ld(Register::X, AddressingMode::Absolute),
                    0xC2 => State::Sep(false),
                    0xE2 => State::Sep(true),
                    0xEA => State::Nop,
                    0xEB => State::Xba,
                    0xFB => State::Xce,
                    _ => todo!("{:02x}", self.ir),
                }
            }
            State::Nop => implied(self, system),
            State::Carry(state) => {
                self.flags.carry = state;
                implied(self, system);
            }
            State::Xce => {
                if !self.aborted {
                    core::mem::swap(&mut self.flags.carry, &mut self.flags.emulation);
                    self.signals.e = self.flags.emulation;

                    if self.flags.emulation {
                        self.flags.mem_sel = true;
                        self.flags.index_sel = true;
                        self.signals.m = true;
                        self.signals.x = true;
                        ByteRef::High(&mut self.s).set(0x01);
                        ByteRef::High(&mut self.x).set(0);
                        ByteRef::High(&mut self.y).set(0);
                    }
                }

                implied(self, system);
            }
            State::Xba => {
                self.signals.mlb = false;
                AddressingMode::Implied.read(self, system);

                if self.tcu == 2 {
                    if !self.aborted {
                        let b = ByteRef::High(&mut self.a).get();
                        let a = ByteRef::Low(&mut self.a).get();
                        ByteRef::High(&mut self.a).set(a);
                        ByteRef::Low(&mut self.a).set(b);

                        self.flags.negative = ((b >> 7) & 1) != 0;
                        self.flags.zero = b != 0;
                    }

                    self.state = State::Fetch;
                }
            }
            State::Tcd => {
                if !self.aborted {
                    self.d = self.a;
                    self.flags.zero = self.d == 0;
                    self.flags.negative = (self.d >> 15) & 1 == 1;
                }

                implied(self, system);
            }
            State::Tcs => {
                if !self.aborted {
                    self.s = self.a;
                    if self.flags.emulation {
                        ByteRef::High(&mut self.s).set(0x01);
                    }
                    self.flags.zero = self.s == 0;
                    self.flags.negative = (self.s >> 15) & 1 == 1;
                }

                implied(self, system);
            }
            State::Tdc => {
                if !self.aborted {
                    self.a = self.d;
                    self.flags.zero = self.a == 0;
                    self.flags.negative = (self.a >> 15) & 1 == 1;
                }

                implied(self, system);
            }
            State::Tsc => {
                if !self.aborted {
                    self.a = self.s;
                    self.flags.zero = self.a == 0;
                    self.flags.negative = (self.a >> 15) & 1 == 1;
                }

                implied(self, system);
            }
            State::Sep(set) => {
                if self.tcu == 1 {
                    if let Some(TaggedByte::Data(Byte::Low(x))) =
                        AddressingMode::Immediate.read(self, system)
                    {
                        ByteRef::Low(&mut self.temp).set(x);
                    }
                } else if self.tcu == 2 {
                    if !self.aborted {
                        self.flags.set_mask(ByteRef::Low(&mut self.temp).get(), set);
                        self.signals.m = self.flags.mem_sel;
                        self.signals.x = self.flags.index_sel;
                    }

                    self.state = State::Fetch;
                }
            }
            State::Ld(reg, addr_mode) => match addr_mode.read(self, system) {
                Some(TaggedByte::Address(Byte::Low(x))) => ByteRef::Low(&mut self.temp).set(x),
                Some(TaggedByte::Address(Byte::High(x))) => ByteRef::High(&mut self.temp).set(x),
                Some(TaggedByte::Data(Byte::Low(x))) => {
                    if !self.aborted {
                        ByteRef::Low(match reg {
                            Register::A => &mut self.a,
                            Register::X => &mut self.x,
                            Register::Y => &mut self.y,
                        })
                        .set(x);

                        self.flags.zero = x == 0;
                        self.flags.negative = (x >> 7) != 0;
                    }

                    if match reg {
                        Register::A => self.a_width(),
                        Register::X | Register::Y => self.index_width(),
                    } == 8
                    {
                        self.state = State::Fetch;
                    }
                }
                Some(TaggedByte::Data(Byte::High(x))) => {
                    if !self.aborted {
                        ByteRef::High(match reg {
                            Register::A => &mut self.a,
                            Register::X => &mut self.x,
                            Register::Y => &mut self.y,
                        })
                        .set(x);

                        self.flags.zero = self.flags.zero && x == 0;
                        self.flags.negative = (x >> 7) != 0;
                    }

                    self.state = State::Fetch;
                }
                _ => (),
            },
            State::St(reg, addr_mode) => match addr_mode.write(
                self,
                system,
                match reg {
                    Register::A => self.a,
                    Register::X => self.x,
                    Register::Y => self.y,
                },
            ) {
                Some(TaggedByte::Address(Byte::Low(x))) => ByteRef::Low(&mut self.temp).set(x),
                Some(TaggedByte::Address(Byte::High(x))) => ByteRef::High(&mut self.temp).set(x),
                Some(TaggedByte::Data(Byte::Low(_x))) => {
                    if match reg {
                        Register::A => self.a_width(),
                        Register::X | Register::Y => self.index_width(),
                    } == 8
                    {
                        self.state = State::Fetch;
                    }
                }
                Some(TaggedByte::Data(Byte::High(_))) => {
                    self.state = State::Fetch;
                }
                _ => (),
            },
            _ => todo!("{:?}", self.state),
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

    /// Returns the width of the accumulator, either 8 or 16
    fn a_width(&self) -> u8 {
        match (self.flags.emulation, self.flags.mem_sel) {
            (true, _) | (false, true) => 8,
            (false, false) => 16,
        }
    }

    /// Returns the width of index registers, either 8 or 16
    fn index_width(&self) -> u8 {
        match (self.flags.emulation, self.flags.index_sel) {
            (true, _) | (false, true) => 8,
            (false, false) => 16,
        }
    }

    /// Push to stack
    fn stack_push(&mut self, system: &mut impl System, byte: Byte, as_read: bool) {
        if as_read {
            system.read(self.s as u32, AddressType::Data, &self.signals);
        } else {
            let byte: u8 = byte.into();
            system.write(self.s as u32, byte, &self.signals);
        }

        if !self.aborted {
            self.s = self.s.wrapping_sub(1);
        }
    }

    /// Pop from stack
    fn stack_pop(&mut self, system: &mut impl System) -> u8 {
        let x = system.read(self.s as u32, AddressType::Data, &self.signals);
        if !self.aborted {
            self.s = self.s.wrapping_sub(1);
        }
        x
    }
}
