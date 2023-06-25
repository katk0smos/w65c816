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
            AddressType::Vector => (true, false),
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

        byte |= Self::BRK_BIT;

        if is_native && self.index_sel {
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
    Relative,
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
    Interrupt {
        vector: u16,
        set_brk: bool,
    },
    Reset,
    Irq,
    Nmi,
    Abort,
    Ld(Register, AddressingMode),
    St(Register, AddressingMode),
    Carry(bool),
    Sep(bool),
    PushAddress(AddressingMode),
    Nop,
    Xce,
    Xba,
    Transfer(Register, Register),
    Txs,
    Tsx,
    Tcd,
    Tcs,
    Tdc,
    Tsc,
    Wai,
    Stp,
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

        if self.wai && !(res || nmi || irq || abort) {
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
            match (cpu.tcu, cpu.flags.emulation) {
                (1, false) => {
                    let effective = ((cpu.pbr as u32) << 16) | cpu.pc as u32;
                    system.read(effective, AddressType::Invalid, &cpu.signals);
                }
                (1, true) | (2, false) => cpu.stack_push(system, cpu.pbr, store_pc_p),
                (2, true) | (3, false) => {
                    let value = ByteRef::High(&mut cpu.pc).get();
                    cpu.stack_push(system, value, store_pc_p);
                }
                (3, true) | (4, false) => {
                    let value = ByteRef::Low(&mut cpu.pc).get();
                    cpu.stack_push(system, value, store_pc_p);
                }
                (4, true) | (5, false) => {
                    let value = cpu.flags.as_byte()
                        & if cpu.flags.emulation && set_b {
                            !Flags::BRK_BIT
                        } else {
                            0xff
                        };
                    cpu.stack_push(system, value, store_pc_p);
                }
                (5, true) | (6, false) => {
                    cpu.flags.interrupt_disable = true;
                    ByteRef::Low(&mut cpu.pc).set(system.read(
                        vector as u32,
                        AddressType::Vector,
                        &cpu.signals,
                    ));
                }
                (6, true) | (7, false) => {
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
            State::Abort => {
                if self.flags.emulation {
                    interrupt(self, system, 0x00fff8, true, false);
                } else {
                    interrupt(self, system, 0x00ffe8, true, false);
                }
            }
            State::Interrupt { vector, set_brk } => interrupt(self, system, vector, true, set_brk),
            State::Fetch => {
                self.signals.mlb = false;
                self.tcu = 0;

                let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                self.ir = system.read(effective, AddressType::Opcode, &self.signals);

                if res {
                    self.ir = 0x00;
                    self.stp = false;
                    self.wai = false;
                    self.state = State::Reset;
                    return;
                } else if nmi {
                    self.ir = 0x00;
                    self.wai = false;
                    self.state = State::Interrupt {
                        vector: if self.flags.emulation { 0xfffa } else { 0xffea },
                        set_brk: false,
                    };
                    return;
                } else if irq {
                    let had_wai = self.wai;
                    self.wai = false;

                    if !had_wai || !self.flags.interrupt_disable {
                        self.ir = 0x00;
                        self.state = State::Interrupt {
                            vector: if self.flags.emulation { 0xfffe } else { 0xffee },
                            set_brk: false,
                        };
                        return;
                    }
                }

                self.pc = self.pc.wrapping_add(1);

                self.state = match self.ir {
                    0x18 => State::Carry(false),
                    0x1B => State::Tcs,
                    0x38 => State::Carry(true),
                    0x3B => State::Tsc,
                    0x5B => State::Tcd,
                    0x62 => State::PushAddress(AddressingMode::Relative),
                    0x7B => State::Tdc,
                    0x84 => State::St(Register::Y, AddressingMode::Direct),
                    0x85 => State::St(Register::A, AddressingMode::Direct),
                    0x86 => State::St(Register::X, AddressingMode::Direct),
                    0x8A => State::Transfer(Register::X, Register::A),
                    0x8C => State::St(Register::Y, AddressingMode::Absolute),
                    0x8D => State::St(Register::A, AddressingMode::Absolute),
                    0x8E => State::St(Register::X, AddressingMode::Absolute),
                    0x98 => State::Transfer(Register::Y, Register::A),
                    0x9A => State::Txs,
                    0x9B => State::Transfer(Register::X, Register::Y),
                    0xA0 => State::Ld(Register::Y, AddressingMode::Immediate),
                    0xA2 => State::Ld(Register::X, AddressingMode::Immediate),
                    0xA4 => State::Ld(Register::Y, AddressingMode::Direct),
                    0xA5 => State::Ld(Register::A, AddressingMode::Direct),
                    0xA6 => State::Ld(Register::X, AddressingMode::Direct),
                    0xA8 => State::Transfer(Register::A, Register::Y),
                    0xA9 => State::Ld(Register::A, AddressingMode::Immediate),
                    0xAA => State::Transfer(Register::A, Register::X),
                    0xAC => State::Ld(Register::Y, AddressingMode::Absolute),
                    0xAD => State::Ld(Register::A, AddressingMode::Absolute),
                    0xAE => State::Ld(Register::X, AddressingMode::Absolute),
                    0xBA => State::Tsx,
                    0xBB => State::Transfer(Register::Y, Register::X),
                    0xC2 => State::Sep(false),
                    0xCB => State::Wai,
                    0xD4 => State::PushAddress(AddressingMode::Direct),
                    0xDB => State::Stp,
                    0xE2 => State::Sep(true),
                    0xEA => State::Nop,
                    0xEB => State::Xba,
                    0xF4 => State::PushAddress(AddressingMode::Immediate),
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
            State::Wai => {
                AddressingMode::Implied.read(self, system);

                if self.tcu == 2 {
                    if !self.aborted {
                        self.wai = true;
                    }

                    self.state = State::Fetch;
                }
            }
            State::Stp => {
                AddressingMode::Implied.read(self, system);

                if self.tcu == 2 {
                    if !self.aborted {
                        self.stp = true;
                    }

                    self.state = State::Fetch;
                }
            }
            State::Transfer(src, dest) => {
                if !self.aborted {
                    let src = match src {
                        Register::A => self.a,
                        Register::X => self.x,
                        Register::Y => self.y,
                    };

                    let (dest_size, dest) = match dest {
                        Register::A => (self.a_width(), &mut self.a),
                        Register::X => (self.index_width(), &mut self.x),
                        Register::Y => (self.index_width(), &mut self.y),
                    };

                    if dest_size == 16 {
                        *dest = src;
                        self.flags.zero = src == 0;
                        self.flags.negative = (src >> 15) & 1 != 0;
                    } else if dest_size == 8 {
                        let mut src = src;
                        let src = ByteRef::Low(&mut src).get();
                        ByteRef::Low(dest).set(src);
                        self.flags.zero = src == 0;
                        self.flags.negative = (src >> 7) & 1 != 0;
                    }
                }

                implied(self, system);
            }
            State::Txs => {
                if !self.aborted {
                    self.s = self.x;
                }

                implied(self, system);
            }
            State::Tsx => {
                if !self.aborted {
                    match self.index_width() {
                        8 => {
                            let value = ByteRef::Low(&mut self.s).get();
                            ByteRef::Low(&mut self.x).set(value);
                            self.flags.zero = value == 0;
                            self.flags.negative = (value >> 7) & 1 != 0;
                        }
                        16 => {
                            self.x = self.s;
                            self.flags.zero = self.x == 0;
                            self.flags.negative = (self.x >> 15) & 1 != 0;
                        }
                        _ => unreachable!(),
                    }
                }

                implied(self, system);
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
            State::PushAddress(addr_mode) => match (self.tcu, addr_mode, self.d != 0) {
                (1, AddressingMode::Absolute, _) => {
                    let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                    let value = system.read(effective, AddressType::Program, &self.signals);
                    self.pc = self.pc.wrapping_add(1);
                    ByteRef::Low(&mut self.temp).set(value);
                }
                (2, AddressingMode::Absolute, _) => {
                    let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                    let value = system.read(effective, AddressType::Program, &self.signals);
                    self.pc = self.pc.wrapping_add(1);
                    ByteRef::High(&mut self.temp).set(value);
                }
                (3, AddressingMode::Absolute, _) => {
                    let value = ByteRef::High(&mut self.temp).get();
                    self.stack_push(system, value, false);
                }
                (4, AddressingMode::Absolute, _) => {
                    let value = ByteRef::Low(&mut self.temp).get();
                    self.stack_push(system, value, false);
                    self.state = State::Fetch;
                }
                (1, AddressingMode::Immediate, _) => {
                    let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                    let value = system.read(effective, AddressType::Program, &self.signals);
                    self.pc = self.pc.wrapping_add(1);
                    self.temp = value as u16;
                }
                (2, AddressingMode::Immediate, true) => {
                    let effective = ((self.pbr as u32) << 16) | (self.pc.wrapping_sub(1) as u32);
                    system.read(effective, AddressType::Invalid, &self.signals);
                }
                (2, AddressingMode::Immediate, false) | (3, AddressingMode::Immediate, true) => {
                    let effective = self.d.wrapping_add(self.temp);
                    let value = system.read(effective as u32, AddressType::Data, &self.signals);
                    // Set high, so we don't overwrite the offset value
                    ByteRef::High(&mut self.temp).set(value);
                }
                (3, AddressingMode::Immediate, false) | (4, AddressingMode::Immediate, true) => {
                    let effective = self
                        .d
                        .wrapping_add(ByteRef::Low(&mut self.temp).get() as u16)
                        .wrapping_add(1);
                    let value =
                        system.read(effective as u32, AddressType::Data, &self.signals) as u16;
                    // Flip temp around when writing the upper byte
                    let temp = ByteRef::High(&mut self.temp).get() as u16;
                    self.temp = temp | (value << 8);
                }
                (4, AddressingMode::Immediate, false) | (5, AddressingMode::Immediate, true) => {
                    let value = ByteRef::High(&mut self.temp).get();
                    self.stack_push(system, value, false);
                }
                (5, AddressingMode::Immediate, false) | (6, AddressingMode::Immediate, true) => {
                    let value = ByteRef::Low(&mut self.temp).get();
                    self.stack_push(system, value, false);
                    self.state = State::Fetch;
                }
                (1, AddressingMode::Relative, _) => {
                    let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                    let value = system.read(effective, AddressType::Program, &self.signals);
                    self.pc = self.pc.wrapping_add(1);
                    ByteRef::Low(&mut self.temp).set(value);
                }
                (2, AddressingMode::Relative, _) => {
                    let effective = ((self.pbr as u32) << 16) | (self.pc as u32);
                    let value = system.read(effective, AddressType::Program, &self.signals);
                    self.pc = self.pc.wrapping_add(1);
                    ByteRef::High(&mut self.temp).set(value);
                }
                (3, AddressingMode::Relative, _) => {
                    let effective = ((self.pbr as u32) << 16) | (self.pc.wrapping_sub(1) as u32);
                    system.read(effective, AddressType::Invalid, &self.signals);
                    self.temp = self.temp.wrapping_add(self.pc);
                }
                (4, AddressingMode::Relative, _) => {
                    let value = ByteRef::High(&mut self.temp).get();
                    self.stack_push(system, value, false);
                }
                (5, AddressingMode::Relative, _) => {
                    let value = ByteRef::Low(&mut self.temp).get();
                    self.stack_push(system, value, false);
                    self.state = State::Fetch;
                }
                _ => {
                    self.state = State::Fetch;
                }
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
    fn stack_push(&mut self, system: &mut impl System, byte: u8, as_read: bool) {
        if as_read {
            system.read(self.s as u32, AddressType::Data, &self.signals);
        } else {
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
