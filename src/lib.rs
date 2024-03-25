#![cfg_attr(not(test), no_std)]
#![cfg_attr(feature = "nightly", feature(test))]
#![cfg(feature = "nightly")]
extern crate test;

#[cfg(test)]
mod tests;

mod util;
use util::*;

mod instructions;

/// Trait that systems should implement.
/// Any given function will be called once per cycle, but not all functions
/// will be called every cycle.
pub trait System {
    fn read(&mut self, addr: u32, addr_type: AddressType, signals: &Signals) -> u8;
    fn write(&mut self, addr: u32, data: u8, addr_type: AddressType, signals: &Signals);
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

/// Extended CPU Register
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ExtRegister {
    A,
    X,
    Y,
    P,
    D,
    Pbr,
    Dbr,
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

        if !is_native || (is_native && self.mem_sel) {
            byte |= Self::MEM_SEL;
        }

        if !is_native {
            byte |= Self::BRK_BIT;
        } else if self.index_sel {
            byte |= Self::INDEX_SEL;
        }

        byte
    }

    pub fn set_mask(&mut self, mask: u8, set: bool) {
        if mask & Self::CARRY != 0 {
            self.carry = set;
        }

        if mask & Self::ZERO != 0 {
            self.zero = set;
        }

        if mask & Self::IRQ_DISABLE != 0 {
            self.interrupt_disable = set;
        }

        if mask & Self::DECIMAL != 0 {
            self.decimal = set;
        }

        if mask & Self::NEGATIVE != 0 {
            self.negative = set;
        }

        if mask & Self::OVERFLOW != 0 {
            self.overflow = set;
        }

        if !self.emulation {
            if mask & Self::MEM_SEL != 0 {
                self.mem_sel = set;
            }

            if mask & Self::INDEX_SEL != 0 {
                self.index_sel = set;
            }
        }
    }

    pub fn set(&mut self, value: u8) {
        self.carry = value & Self::CARRY != 0;
        self.zero = value & Self::ZERO != 0;
        self.interrupt_disable = value & Self::IRQ_DISABLE != 0;
        self.decimal = value & Self::DECIMAL != 0;
        self.negative = value & Self::NEGATIVE != 0;
        self.overflow = value & Self::OVERFLOW != 0;
        if !self.emulation {
            self.mem_sel = value & Self::MEM_SEL != 0;
            self.index_sel = value & Self::INDEX_SEL != 0;
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Default)]
pub(crate) enum AddressingMode {
    #[default]
    Implied,
    Accumulator,
    Immediate,
    Absolute,
    AbsoluteIndexedX,
    AbsoluteIndexedY,
    Direct,
    DirectIndirect,
    DirectIndirectLong,
    DirectIndirectLongIndexedY,
    DirectIndexedX,
    DirectIndexedY,
    DirectIndirectX,
    DirectIndirectIndexedY,
    Relative,
    StackRel,
    StackRelIndirectIndexedY,
    AbsoluteLong,
    AbsoluteLongIndexedX,
    IndexedIndirectX,
}

impl AddressingMode {
    pub fn read(self, cpu: &mut CPU, system: &mut dyn System) -> Option<TaggedByte> {
        match self {
            AddressingMode::Immediate => match (cpu.flags.emulation, cpu.tcu) {
                (_, 1) => {
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
                    let effective = ((cpu.dbr as u32) << 16) | (cpu.temp_addr as u32);
                    let value = system.read(effective, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 4) => {
                    let effective =
                        (((cpu.dbr as u32) << 16) | (cpu.temp_addr as u32)).wrapping_add(1);
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
                    let addr = cpu
                        .d
                        .wrapping_add(ByteRef::Low(&mut cpu.temp_addr).get() as u16)
                        as u32;
                    let value = system.read(addr, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 3, true) | (false, 4, false) => {
                    let addr = cpu
                        .d
                        .wrapping_add(ByteRef::Low(&mut cpu.temp_addr).get() as u16)
                        .wrapping_add(1) as u32;
                    let value = system.read(addr, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::High(value)))
                }
                _ => None,
            },
            AddressingMode::StackRel => match cpu.tcu {
                1 => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let offset = system.read(effective, AddressType::Program, &cpu.signals);
                    cpu.pc = cpu.pc.wrapping_add(1);
                    
                    Some(TaggedByte::Address(Byte::Low(offset)))
                }
                2 => {
                    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
                    let _ = system.read(effective, AddressType::Invalid, &cpu.signals);
                    None
                }
                3 => {
                    let offset = ByteRef::Low(&mut cpu.temp_addr).get() as u16;
                    let effective = if cpu.flags.emulation {
                        let mut s = ByteRef::Low(&mut cpu.s).get() as u16 + offset;
                        (1 << 8) | ByteRef::Low(&mut s).get() as u16
                    } else {
                        cpu.s.wrapping_add(offset)
                    };
                    
                    let data = system.read(effective as u32, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(data)))
                }
                4 => {
                    let offset = ByteRef::Low(&mut cpu.temp_addr).get() as u16 + 1;
                    let effective = if cpu.flags.emulation {
                        let mut s = ByteRef::Low(&mut cpu.s).get() as u16 + offset;
                        (1 << 8) | ByteRef::Low(&mut s).get() as u16
                    } else {
                        cpu.s.wrapping_add(offset)
                    };
                    
                    let data = system.read(effective as u32, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::High(data)))
                }
                _ => None,
            }
            _ => todo!(),
        }
    }

    pub fn write(
        self,
        cpu: &mut CPU,
        system: &mut dyn System,
        mut value: u16,
    ) -> Option<TaggedByte> {
        match self {
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
                    let effective = ((cpu.dbr as u32) << 16) | (cpu.temp_addr as u32);
                    let value = ByteRef::Low(&mut value).get();
                    system.write(effective, value, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 4) => {
                    let effective = ((cpu.dbr as u32) << 16) | (cpu.temp_addr.wrapping_add(1) as u32);
                    let value = ByteRef::High(&mut value).get();
                    system.write(effective, value, AddressType::Data, &cpu.signals);
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
                    let addr = cpu
                        .d
                        .wrapping_add(ByteRef::Low(&mut cpu.temp_addr).get() as u16)
                        as u32;
                    let value = ByteRef::Low(&mut value).get();
                    system.write(addr, value, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::Low(value)))
                }
                (false, 3, true) | (false, 4, false) => {
                    let addr = cpu
                        .d
                        .wrapping_add(ByteRef::Low(&mut cpu.temp_addr).get() as u16)
                        .wrapping_add(1) as u32;
                    let value = ByteRef::High(&mut value).get();
                    system.write(addr, value, AddressType::Data, &cpu.signals);
                    Some(TaggedByte::Data(Byte::High(value)))
                }
                _ => None,
            },
            AddressingMode::Implied | AddressingMode::Immediate => None,
            _ => todo!(),
        }
    }
}

/// List of conditions
#[derive(Clone, Debug, Copy, PartialEq, Default)]
pub(crate) enum Condition {
    #[default]
    Always,
    Carry(bool),
    Equal(bool),
    Minus(bool),
    Overflow(bool),
}

/// Internal state machine
#[derive(Clone, Debug, Copy, PartialEq, Default)]
pub(crate) enum State {
    #[default]
    Fetch,
    Interrupt {
        vector: u16,
        set_brk: bool,
    },
    Reset,
    Abort,
    Brk,
    // Instructions
    Instruction(instructions::InstructionFn, AddressingMode),
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
    /// Temporary scratch register for internal use.
    /// Used for addressing instructions
    temp_addr: u16,
    /// Temporary scratch register for internal use.
    /// Used for addressing instructions
    temp_data: u16,
    /// Tempoary scratch register for internal use.
    /// Used for storing a bank address
    temp_bank: u8,
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
            temp_addr: 0,
            temp_data: 0,
            temp_bank: 0,
        }
    }
}

impl CPU {
    #[inline(always)]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn cycle(&mut self, system: &mut dyn System) {
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

        fn implied(cpu: &mut CPU, system: &mut dyn System) {
            cpu.signals.mlb = false;
            let _ = AddressingMode::Implied.read(cpu, system);
            cpu.state = State::Fetch;
        }

        fn interrupt(
            cpu: &mut CPU,
            system: &mut dyn System,
            vector: u16,
            store_pc_p: bool,
            set_b: bool,
        ) {
            match (cpu.tcu, cpu.flags.emulation) {
                (1, _) => {
                    let effective = ((cpu.pbr as u32) << 16) | cpu.pc as u32;
                    system.read(effective, AddressType::Invalid, &cpu.signals);
                }
                (2, false) => cpu.stack_push(system, cpu.pbr, !store_pc_p),
                (2, true) | (3, false) => {
                    let value = ByteRef::High(&mut cpu.pc).get();
                    cpu.stack_push(system, value, !store_pc_p);
                }
                (3, true) | (4, false) => {
                    let value = ByteRef::Low(&mut cpu.pc).get();
                    cpu.stack_push(system, value, !store_pc_p);
                }
                (4, true) | (5, false) => {
                    let value = cpu.flags.as_byte()
                        & if cpu.flags.emulation && !set_b {
                            !Flags::BRK_BIT
                        } else {
                            0xff
                        };
                    cpu.stack_push(system, value, !store_pc_p);
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

                    cpu.pbr = 0;
                    cpu.dbr = 0;
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
            State::Brk => {
                let vector = if self.flags.emulation { 0xfffe } else { 0xffe6 };

                match (self.tcu, self.flags.emulation) {
                    (1, _) => {
                        let effective = ((self.pbr as u32) << 16) | self.pc as u32;
                        system.read(effective, AddressType::Program, &self.signals);
                        self.pc = self.pc.wrapping_add(1);
                    }
                    (5, true) | (6, false) => {
                        self.flags.decimal = false;
                        self.flags.interrupt_disable = true;
                        interrupt(self, system, vector, true, true);
                    }
                    (7, false) => {
                        let dbr = self.dbr;
                        interrupt(self, system, vector, true, true);
                        self.dbr = dbr;
                    }
                    _ => interrupt(self, system, vector, true, true),
                }
            }
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
                    0 => State::Brk,
                    ir => {
                        let (f, am) = instructions::INSTRUCTIONS[ir as usize];
                        State::Instruction(f, am)
                    }
                }
            }
            State::Instruction(f, am) => {
                f(self, system, am)
            }
            _ => todo!("{:?}", self.state),
        }
    }

    /// RDY signal, with internal pulldown.
    #[inline(always)]
    pub fn rdy(&self, system: &mut dyn System) -> bool {
        system.rdy() & self.rdy
    }

    /// Signals
    #[inline(always)]
    pub fn signals(&self) -> &Signals {
        &self.signals
    }

    /// Flags
    #[inline(always)]
    pub fn flags(&self) -> &Flags {
        &self.flags
    }

    /// Returns the width of the accumulator, either 8 or 16
    #[inline(always)]
    fn a_width(&self) -> u8 {
        match (self.flags.emulation, self.flags.mem_sel) {
            (true, _) | (false, true) => 8,
            (false, false) => 16,
        }
    }

    #[inline(always)]
    fn a8(&self) -> bool {
        self.a_width() == 8
    }

    #[inline(always)]
    fn a16(&self) -> bool {
        self.a_width() == 16
    }
    
    /// Returns the width of index registers, either 8 or 16
    #[inline(always)]
    fn index_width(&self) -> u8 {
        match (self.flags.emulation, self.flags.index_sel) {
            (true, _) | (false, true) => 8,
            (false, false) => 16,
        }
    }
    
    #[inline(always)]
    fn m8(&self) -> bool {
        self.index_width() == 8
    }
    
    #[inline(always)]
    fn m16(&self) -> bool {
        self.index_width() == 16
    }

    /// Push to stack
    fn stack_push(&mut self, system: &mut dyn System, byte: u8, as_read: bool) {
        if as_read {
            system.read(self.s as u32, AddressType::Data, &self.signals);
        } else {
            system.write(self.s as u32, byte, AddressType::Data, &self.signals);
        }

        if !self.aborted {
            self.s = self.s.wrapping_sub(1);

            if self.flags.emulation {
                ByteRef::High(&mut self.s).set(0x01);
            }
        }
    }

    /// Pop from stack
    fn stack_pop(&mut self, system: &mut dyn System) -> u8 {
        if !self.aborted {
            self.s = self.s.wrapping_add(1);

            if self.flags.emulation {
                ByteRef::High(&mut self.s).set(0x01);
            }
        }

        system.read(self.s as u32, AddressType::Data, &self.signals)
    }

    /// Sets the emulation flag state
    pub fn set_e(&mut self, e: bool) {
        self.flags.emulation = e;
        self.signals.e = e;

        if e {
            self.flags.mem_sel = true;
            self.flags.index_sel = true;
            self.signals.m = true;
            self.signals.x = true;
            ByteRef::High(&mut self.x).set(0);
            ByteRef::High(&mut self.y).set(0);
        }
    }

    /// Sets the program bank register
    #[inline(always)]
    pub fn set_pbr(&mut self, pbr: u8) {
        self.pbr = pbr;
    }

    /// Sets the program bank register
    #[inline(always)]
    pub fn set_dbr(&mut self, dbr: u8) {
        self.dbr = dbr;
    }

    /// Set the program counter
    #[inline(always)]
    pub fn set_pc(&mut self, pc: u16) {
        self.pc = pc;
    }

    /// Set the stack pointer
    #[inline(always)]
    pub fn set_s(&mut self, s: u16) {
        self.s = s;

        if self.flags.emulation {
            ByteRef::High(&mut self.s).set(0x01);
        }
    }

    /// Sets the flag register
    #[inline(always)]
    pub fn set_p(&mut self, p: u8) {
        self.flags.set(p);
        self.signals.m = self.flags.mem_sel;
        self.signals.x = self.flags.index_sel;
        if self.m8() {
            ByteRef::High(&mut self.x).set(0);
            ByteRef::High(&mut self.y).set(0);
        }
    }

    #[inline(always)]
    pub fn set_acc(&mut self, mut acc: u16) {
        if self.a_width() == 8 {
            ByteRef::Low(&mut self.a).set(ByteRef::Low(&mut acc).get());
            ByteRef::High(&mut self.a).set(0);
        } else {
            self.a = acc;
        }
    }

    /// Sets the accumulator's low byte
    #[inline(always)]
    pub fn set_a(&mut self, a: u8) {
        ByteRef::Low(&mut self.a).set(a);
    }

    /// Sets the accumulator's high byte
    #[inline(always)]
    pub fn set_b(&mut self, b: u8) {
        ByteRef::High(&mut self.a).set(b);
    }

    /// Sets the accumulator's entire word
    #[inline(always)]
    pub fn set_c(&mut self, c: u16) {
        self.a = c;
    }

    /// Sets the X index register
    pub fn set_x(&mut self, mut x: u16) {
        if self.m8() {
            ByteRef::Low(&mut self.x).set(ByteRef::Low(&mut x).get());
            ByteRef::High(&mut self.x).set(0);
        } else {
            self.x = x;
        }
    }

    /// Sets the Y index register
    pub fn set_y(&mut self, mut y: u16) {
        if self.m8() {
            ByteRef::Low(&mut self.y).set(ByteRef::Low(&mut y).get());
            ByteRef::High(&mut self.x).set(0);
        } else {
            self.y = y;
        }
    }

    /// Sets the Direct register (D)
    #[inline(always)]
    pub fn set_d(&mut self, d: u16) {
        self.d = d;
    }

    /// Returns the processor status flags register (P)
    #[inline(always)]
    pub fn p(&self) -> u8 {
        self.flags.as_byte()
    }

    /// Returns the Direct register (D)
    #[inline(always)]
    pub fn d(&self) -> u16 {
        self.d
    }

    /// Returns the X index register
    #[inline(always)]
    pub fn x(&self) -> u16 {
        self.x
    }

    /// Returns the Y index register
    #[inline(always)]
    pub fn y(&self) -> u16 {
        self.y
    }

    /// Returns the stack pointer
    #[inline(always)]
    pub fn s(&self) -> u16 {
        self.s
    }

    /// Returns the program counter
    #[inline(always)]
    pub fn pc(&self) -> u16 {
        self.pc
    }

    /// Returns the program bank register
    #[inline(always)]
    pub fn pbr(&self) -> u8 {
        self.pbr
    }

    /// Returns the K register (pbr/program bank register)
    #[inline(always)]
    pub fn k(&self) -> u8 {
        self.pbr
    }

    /// Returns the data bank register
    #[inline(always)]
    pub fn dbr(&self) -> u8 {
        self.dbr
    }

    /// Returns the accumulator
    #[inline(always)]
    pub fn c(&self) -> u16 {
        self.a
    }

    /// Returns the accumulator's low byte
    #[inline(always)]
    pub fn a(&self) -> u8 {
        self.a as u8
    }

    /// Returns the accumulator's high byte
    #[inline(always)]
    pub fn b(&self) -> u8 {
        (self.a >> 8) as u8
    }
}
