use super::{CPU, System, AddressType, AddressingMode, State, ByteRef, Byte, TaggedByte};

pub type InstructionFn = fn(&mut CPU, &mut dyn System, AddressingMode) -> ();

fn todo(cpu: &mut CPU, _sys: &mut dyn System, _am: AddressingMode) {
    todo!("{:02x}{:04x} {:02x}", cpu.pbr, cpu.pc, cpu.ir);
}

#[inline(always)]
fn io(cpu: &mut CPU, sys: &mut dyn System) {
    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
    let _ = sys.read(effective, AddressType::Invalid, &cpu.signals);
}

#[inline(always)]
fn implied(cpu: &mut CPU, sys: &mut dyn System) {
    io(cpu, sys);

    if cpu.tcu == 1 {
        cpu.state = State::Fetch;
    }
}

// doesn't need to be implemented, implemented as a state in the state machine
// so this is never actually called
fn brk_cop(_cpu: &mut CPU, _sys: &mut dyn System, _am: AddressingMode) {}

fn stp(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    io(cpu, sys);

    if cpu.tcu == 2 {
        if !cpu.aborted {
            cpu.stp = true;
        }

        cpu.state = State::Fetch;
    }
}

fn wai(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    io(cpu, sys);

    if cpu.tcu == 2 {
        if !cpu.aborted {
            cpu.wai = true;
        }

        cpu.state = State::Fetch;
    }
}

fn nop(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
}

fn clc(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if !cpu.aborted {
        cpu.flags.carry = false;
    }
}

fn sec(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if !cpu.aborted {
        cpu.flags.carry = true;
    }
}

fn cli(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if !cpu.aborted {
        cpu.flags.interrupt_disable = false;
    }
}

fn sei(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if !cpu.aborted {
        cpu.flags.interrupt_disable = true;
    }
}

fn cld(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if !cpu.aborted {
        cpu.flags.decimal = false;
    }
}

fn sed(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if !cpu.aborted {
        cpu.flags.decimal = true;
    }
}

fn clv(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if !cpu.aborted {
        cpu.flags.overflow = false;
    }
}

fn xce(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if cpu.tcu == 1 && !cpu.aborted {
        let old_e = cpu.flags.emulation;
        cpu.flags.emulation = cpu.flags.carry;
        cpu.flags.carry = old_e;
        cpu.set_e(cpu.flags.emulation);
    }
}

fn lda(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            ByteRef::Low(&mut cpu.a).set(l);
            cpu.flags.zero = l == 0;
            if cpu.a8() {
                cpu.state = State::Fetch;
                cpu.flags.negative = l >> 7 != 0;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            ByteRef::High(&mut cpu.a).set(h);
            cpu.state = State::Fetch;
            cpu.flags.negative = h >> 7 != 0;
            cpu.flags.zero = cpu.flags.zero && h == 0;
        }
        _ => (),
    }
}

fn ldx(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            ByteRef::Low(&mut cpu.x).set(l);
            cpu.flags.zero = l == 0;
            if cpu.m8() {
                cpu.state = State::Fetch;
                cpu.flags.negative = l >> 7 != 0;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            ByteRef::High(&mut cpu.x).set(h);
            cpu.state = State::Fetch;
            cpu.flags.negative = h >> 7 != 0;
            cpu.flags.zero = cpu.flags.zero && h == 0;
        }
        _ => (),
    }
}

fn ldy(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            ByteRef::Low(&mut cpu.y).set(l);
            cpu.flags.zero = l == 0;
            if cpu.m8() {
                cpu.state = State::Fetch;
                cpu.flags.negative = l >> 7 != 0;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            ByteRef::High(&mut cpu.y).set(h);
            cpu.state = State::Fetch;
            cpu.flags.negative = h >> 7 != 0;
            cpu.flags.zero = cpu.flags.zero && h == 0;
        }
        _ => (),
    }
}

fn sta(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.write(cpu, sys, cpu.a) {
        Some(TaggedByte::Data(Byte::Low(_))) if cpu.a8() => {
            cpu.state = State::Fetch;
        }
        Some(TaggedByte::Data(Byte::High(_))) => {
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn stx(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.write(cpu, sys, cpu.x) {
        Some(TaggedByte::Data(Byte::Low(_))) if cpu.m8() => {
            cpu.state = State::Fetch;
        }
        Some(TaggedByte::Data(Byte::High(_))) => {
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn sty(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.write(cpu, sys, cpu.y) {
        Some(TaggedByte::Data(Byte::Low(_))) if cpu.m8() => {
            cpu.state = State::Fetch;
        }
        Some(TaggedByte::Data(Byte::High(_))) => {
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn stz(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.write(cpu, sys, 0) {
        Some(TaggedByte::Data(Byte::Low(_))) if cpu.a8() => {
            cpu.state = State::Fetch;
        }
        Some(TaggedByte::Data(Byte::High(_))) => {
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

#[inline]
fn jsr_al(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    match cpu.tcu {
        1 => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::Low(&mut cpu.temp_addr).set(data);
            cpu.pc = cpu.pc.wrapping_add(1);
        }
        2 => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::High(&mut cpu.temp_addr).set(data);
            cpu.pc = cpu.pc.wrapping_add(1);
        }
        3 => {
            cpu.stack_push(sys, cpu.pbr, false);
        }
        4 => {
            let mut s = cpu.s.wrapping_add(1);
            if cpu.flags.emulation {
                ByteRef::High(&mut s).set(0x01);
            }
            sys.read(s as u32, AddressType::Invalid, &cpu.signals);
        }
        5 => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            cpu.temp_bank = data;
        }
        6 => {
            let pc = ByteRef::High(&mut cpu.pc).get();
            cpu.stack_push(sys, pc, false);
        }
        7 => {
            let pc = ByteRef::Low(&mut cpu.pc).get();
            cpu.stack_push(sys, pc, false);
            cpu.pc = cpu.temp_addr;
            cpu.pbr = cpu.temp_bank;
            cpu.state = State::Fetch;
        }
        _ => unreachable!(),
    }
}

fn jsr(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    const ABS: AddressingMode = AddressingMode::Absolute;
    const IDX_IN: AddressingMode = AddressingMode::IndexedIndirectX;

    match (cpu.tcu, am) {
        (1, ABS) | (1, IDX_IN) => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::Low(&mut cpu.temp_addr).set(data);
            cpu.pc = cpu.pc.wrapping_add(1);
        }
        (2, ABS) | (4, IDX_IN) => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::High(&mut cpu.temp_addr).set(data);
        }
        (3, ABS) | (5, IDX_IN) => {
            io(cpu, sys)
        }
        (4, ABS) | (2, IDX_IN) => {
            let pc = ByteRef::High(&mut cpu.pc).get();
            cpu.stack_push(sys, pc, false);
        }
        (5, ABS) | (3, IDX_IN) => {
            let pc = ByteRef::Low(&mut cpu.pc).get();
            cpu.stack_push(sys, pc, false);
            if am == ABS {
                cpu.pc = cpu.temp_addr;
                cpu.state = State::Fetch;
            }
        }
        (6, IDX_IN) => {
            // pbr,aa+x
            let pc = ((cpu.pbr as u32) << 16) | ((cpu.temp_addr.wrapping_add(cpu.x)) as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::Low(&mut cpu.temp_data).set(data);
        }
        (7, IDX_IN) => {
            // pbr,aa+x+1
            let pc = ((cpu.pbr as u32) << 16) | ((cpu.temp_addr.wrapping_add(cpu.x).wrapping_add(1)) as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::High(&mut cpu.temp_data).set(data);
            cpu.pc = cpu.temp_data;
            cpu.state = State::Fetch;
        }
        _ => unreachable!(),
    }
}

fn jml(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    match cpu.tcu {
        1 => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::Low(&mut cpu.temp_addr).set(data);
            cpu.pc = cpu.pc.wrapping_add(1);
        }
        2 => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::High(&mut cpu.temp_addr).set(data);
            cpu.pc = cpu.pc.wrapping_add(1);
        }
        3 => {
            let pc = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            cpu.pbr = data;
            cpu.pc = cpu.temp_addr;
            cpu.state = State::Fetch;
        }
        _ => unreachable!(),
    }
}

fn jmp(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am {
        AddressingMode::IndirectLong => {
            todo!("jmp [abs]")
        }
        _ => (),
    }

    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => cpu.temp_data = l as u16,
        Some(TaggedByte::Data(Byte::High(h))) => {
            ByteRef::High(&mut cpu.temp_data).set(h);
            cpu.pc = cpu.temp_data;
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn sep(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
    let data = match cpu.tcu {
        1 => {
            cpu.pc = cpu.pc.wrapping_add(1);
            sys.read(effective, AddressType::Program, &cpu.signals)
        }
        2 => sys.read(effective, AddressType::Invalid, &cpu.signals),
        _ => unreachable!(),
    };

    if cpu.tcu == 1 && !cpu.aborted {
        cpu.flags.set_mask(data, true);
        cpu.signals.m = cpu.flags.mem_sel;
        cpu.signals.x = cpu.flags.index_sel;
        if cpu.flags.index_sel {
            ByteRef::High(&mut cpu.x).set(0);
            ByteRef::High(&mut cpu.y).set(0);
        }
    }

    if cpu.tcu == 2 {
        cpu.state = State::Fetch;
    }
}

fn rep(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
    let data = match cpu.tcu {
        1 => {
            cpu.pc = cpu.pc.wrapping_add(1);
            sys.read(effective, AddressType::Program, &cpu.signals)
        }
        2 => sys.read(effective, AddressType::Invalid, &cpu.signals),
        _ => unreachable!(),
    };

    if cpu.tcu == 1 && !cpu.aborted {
        cpu.flags.set_mask(data, false);
        cpu.signals.m = cpu.flags.mem_sel;
        cpu.signals.x = cpu.flags.index_sel;
        if cpu.flags.index_sel {
            ByteRef::High(&mut cpu.x).set(0);
            ByteRef::High(&mut cpu.y).set(0);
        }
    }

    if cpu.tcu == 2 {
        cpu.state = State::Fetch;
    }
}

fn rts(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    const RTS: AddressingMode = AddressingMode::Absolute;
    const RTL: AddressingMode = AddressingMode::AbsoluteLong;

    match (am, cpu.tcu) {
        (_, 1 | 2) => io(cpu, sys),
        (_, 3) => {
            let data = cpu.stack_pop(sys);
            ByteRef::Low(&mut cpu.pc).set(data);
        }
        (_, 4) => {
            let data = cpu.stack_pop(sys);
            ByteRef::High(&mut cpu.pc).set(data);
        }
        (RTS, 5) => {
            let mut s = cpu.s;
            if cpu.flags.emulation {
                ByteRef::High(&mut s).set(0x01);
            }
            sys.read(s as u32, AddressType::Invalid, &cpu.signals);
            cpu.pc = cpu.pc.wrapping_add(1);
            cpu.state = State::Fetch;
        }
        (RTL, 5) => {
            let data = cpu.stack_pop(sys);
            cpu.pbr = data;
            cpu.pc = cpu.pc.wrapping_add(1);
            cpu.state = State::Fetch;
        }
        _ => todo!()
    }
}

fn rti(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    match cpu.tcu {
        1 | 2 => io(cpu, sys),
        3 => {
            let data = cpu.stack_pop(sys);
            cpu.set_p(data);
        }
        4 => {
            let data = cpu.stack_pop(sys);
            ByteRef::Low(&mut cpu.pc).set(data);
        }
        5 => {
            let data = cpu.stack_pop(sys);
            ByteRef::High(&mut cpu.pc).set(data);
        }
        6 => {
            let data = cpu.stack_pop(sys);
            cpu.pbr = data;
            cpu.state = State::Fetch;
        }
        _ => unreachable!(),
    }
}

macro_rules! transfer {
    ($name:ident, $cpu:ident, $cond8:expr, $r0:expr, $r1:expr) => {
        fn $name($cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
            implied($cpu, sys);
            if $cpu.tcu == 2 {
                if !$cpu.aborted {
                    if $cond8 {
                        let data = ByteRef::Low(&mut $r0).get();
                        ByteRef::Low(&mut $r1).set(data);
                    } else {
                        $r1 = $r0;
                    }
                }

                $cpu.state = State::Fetch;
            }
        }
    }
}

transfer!(tax, cpu, cpu.m8(), cpu.a, cpu.x);
transfer!(tay, cpu, cpu.m8(), cpu.a, cpu.y);
transfer!(tsx, cpu, cpu.flags.emulation, cpu.s, cpu.x);
transfer!(txa, cpu, cpu.a8(), cpu.x, cpu.a);
transfer!(txs, cpu, cpu.flags.emulation, cpu.x, cpu.s);
transfer!(txy, cpu, cpu.flags.emulation, cpu.x, cpu.y);
transfer!(tya, cpu, cpu.a8(), cpu.y, cpu.a);
transfer!(tyx, cpu, cpu.flags.emulation, cpu.y, cpu.x);

macro_rules! transfer16 {
    ($name:ident, $cpu:ident, $r0:expr, $r1:expr) => {
        fn $name($cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
            implied($cpu, sys);
            if !$cpu.aborted {
                $r1 = $r0;
            }
            $cpu.state = State::Fetch;
        }
    }
}

transfer16!(tcd, cpu, cpu.a, cpu.d);
transfer16!(tcs, cpu, cpu.a, cpu.s);
transfer16!(tdc, cpu, cpu.d, cpu.a);
transfer16!(tsc, cpu, cpu.s, cpu.a);

fn adc(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            let l = l as u16;
            let c = if cpu.flags.carry { 1 } else { 0 };
            let a = ByteRef::Low(&mut cpu.a).get() as u16;
            let mut r = a.wrapping_add(l).wrapping_add(c);
            // handle decimal mode
            if cpu.flags.decimal {
                if (a ^ l ^ r) & 0x10 != 0 {
                    r = r.wrapping_add(6);
                }

                if (r & 0xf0) > 0x90 {
                    r = r.wrapping_add(0x60);
                }
            }
            ByteRef::Low(&mut cpu.a).set(r as u8);
            cpu.flags.carry = r > 0xff;
            cpu.flags.zero = r == 0;
            cpu.flags.negative = r & 0x80 != 0;
            cpu.flags.overflow = (a ^ r) & (l ^ r) & 0x80 != 0;
            if cpu.a8() {
                cpu.state = State::Fetch;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            let h = h as u16;
            let c = if cpu.flags.carry { 1 } else { 0 };
            let a = ByteRef::High(&mut cpu.a).get() as u16;
            let mut r = a.wrapping_add(h).wrapping_add(c);
            // handle decimal mode
            if cpu.flags.decimal {
                if (a ^ h ^ r) & 0x10 != 0 {
                    r = r.wrapping_add(6);
                }

                if (r & 0xf0) > 0x90 {
                    r = r.wrapping_add(0x60);
                }
            }
            ByteRef::High(&mut cpu.a).set(r as u8);
            cpu.flags.carry = r > 0xff;
            cpu.flags.zero = cpu.flags.zero && r == 0;
            cpu.flags.negative = r & 0x80 != 0;
            cpu.flags.overflow = (a ^ r) & (h ^ r) & 0x80 != 0;
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn sbc(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            let l = (!l) as u16;
            let c = if cpu.flags.carry { 1 } else { 0 };
            let a = ByteRef::Low(&mut cpu.a).get() as u16;
            let mut r = a.wrapping_add(l).wrapping_add(c);
            // handle decimal mode
            if cpu.flags.decimal {
                if (a ^ l ^ r) & 0x10 != 0 {
                    r = r.wrapping_add(6);
                }

                if (r & 0xf0) > 0x90 {
                    r = r.wrapping_add(0x60);
                }
            }
            ByteRef::Low(&mut cpu.a).set(r as u8);
            cpu.flags.carry = r > 0xff;
            cpu.flags.zero = r == 0;
            cpu.flags.negative = r & 0x80 != 0;
            cpu.flags.overflow = (a ^ r) & (l ^ r) & 0x80 != 0;
            if cpu.a8() {
                cpu.state = State::Fetch;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            let h = (!h) as u16;
            let c = if cpu.flags.carry { 1 } else { 0 };
            let a = ByteRef::High(&mut cpu.a).get() as u16;
            let mut r = a.wrapping_add(h).wrapping_add(c);
            // handle decimal mode
            if cpu.flags.decimal {
                if (a ^ h ^ r) & 0x10 != 0 {
                    r = r.wrapping_add(6);
                }

                if (r & 0xf0) > 0x90 {
                    r = r.wrapping_add(0x60);
                }
            }
            ByteRef::High(&mut cpu.a).set(r as u8);
            cpu.flags.carry = r > 0xff;
            cpu.flags.zero = cpu.flags.zero && r == 0;
            cpu.flags.negative = r & 0x80 != 0;
            cpu.flags.overflow = (a ^ r) & (h ^ r) & 0x80 != 0;
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn and(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            let a = ByteRef::Low(&mut cpu.a).get() & l;
            ByteRef::Low(&mut cpu.a).set(a);
            cpu.flags.zero = a == 0;
            if cpu.a8() {
                cpu.flags.negative = a >> 7 != 0;
                cpu.state = State::Fetch;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            let a = ByteRef::High(&mut cpu.a).get() & h;
            ByteRef::High(&mut cpu.a).set(a);
            cpu.flags.zero = cpu.flags.zero && a == 0;
            cpu.flags.negative = a >> 7 != 0;
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn ora(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            let a = ByteRef::Low(&mut cpu.a).get() | l;
            ByteRef::Low(&mut cpu.a).set(a);
            cpu.flags.zero = a == 0;
            if cpu.a8() {
                cpu.flags.negative = a >> 7 != 0;
                cpu.state = State::Fetch;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            let a = ByteRef::High(&mut cpu.a).get() | h;
            ByteRef::High(&mut cpu.a).set(a);
            cpu.flags.zero = cpu.flags.zero && a == 0;
            cpu.flags.negative = a >> 7 != 0;
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

fn eor(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    match am.read(cpu, sys) {
        Some(TaggedByte::Data(Byte::Low(l))) => {
            let a = ByteRef::Low(&mut cpu.a).get() ^ l;
            ByteRef::Low(&mut cpu.a).set(a);
            cpu.flags.zero = a == 0;
            if cpu.a8() {
                cpu.flags.negative = a >> 7 != 0;
                cpu.state = State::Fetch;
            }
        }
        Some(TaggedByte::Data(Byte::High(h))) => {
            let a = ByteRef::High(&mut cpu.a).get() ^ h;
            ByteRef::High(&mut cpu.a).set(a);
            cpu.flags.zero = cpu.flags.zero && a == 0;
            cpu.flags.negative = a >> 7 != 0;
            cpu.state = State::Fetch;
        }
        _ => (),
    }
}

macro_rules! branch {
    ($name:ident, $cpu:ident, $expr:expr) => {
        fn $name($cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
            match $cpu.tcu {
                1 => {
                    let effective = (($cpu.pbr as u32) << 16) | ($cpu.pc as u32);
                    let offset = sys.read(effective, AddressType::Program, &$cpu.signals);
                    $cpu.temp_addr = offset as u16;
                    $cpu.pc = $cpu.pc.wrapping_add(1);
                    $cpu.temp_data = $cpu.pc;

                    if !($expr) {
                        $cpu.state = State::Fetch;
                    }
                }
                2 => {
                    io($cpu, sys);
                    let ba = $cpu.pc.wrapping_add_signed(($cpu.temp_addr as i8).into());
                    $cpu.pc = ba;
                    if !$cpu.flags.emulation || $cpu.pc & 0xff00 == ba & 0xff00 {
                        $cpu.state = State::Fetch;
                    }
                }
                3 => {
                    let effective = (($cpu.pbr as u32) << 16) | ($cpu.temp_data as u32);
                    sys.read(effective, AddressType::Invalid, &$cpu.signals);
                    $cpu.state = State::Fetch;
                }
                _ => unreachable!(),
            }
        }
    }
}

branch!(bcc, cpu, !cpu.flags.carry);
branch!(bcs, cpu, cpu.flags.carry);
branch!(bne, cpu, !cpu.flags.zero);
branch!(beq, cpu, cpu.flags.zero);
branch!(bpl, cpu, !cpu.flags.negative);
branch!(bmi, cpu, cpu.flags.negative);
branch!(bvc, cpu, !cpu.flags.overflow);
branch!(bvs, cpu, cpu.flags.overflow);
branch!(bra, cpu, true);

macro_rules! push {
    ($name:ident, $cpu:ident, $r:expr, $b16:expr) => {
        fn $name($cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
            let mut r: u16 = $r;
            match $cpu.tcu {
                1 if $b16 => {
                    let b = ByteRef::High(&mut r).get();
                    $cpu.stack_push(sys, b, false);
                }
                1 | 2 => {
                    let b = ByteRef::Low(&mut r).get();
                    $cpu.stack_push(sys, b, false);
                    $cpu.state = State::Fetch;
                }
                _ => unreachable!(),
            }
        }
    }
}

macro_rules! pull8 {
    ($name:ident, $cpu:ident, $v:ident, $set:stmt) => {
        fn $name($cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
            match $cpu.tcu {
                1 => {
                    let $v = $cpu.stack_pop(sys);
                    $set;
                    $cpu.state = State::Fetch;
                }
                _ => unreachable!(),
            }
        }
    }
}

macro_rules! pull16 {
    ($name:ident, $cpu:ident, $r:expr, $b16:expr) => {
        fn $name($cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
            match $cpu.tcu {
                1 => {
                    let b = $cpu.stack_pop(sys);
                    ByteRef::Low(&mut $r).set(b);
                    if !($b16) {
                        $cpu.state = State::Fetch;
                    }
                }
                2 => {
                    let b = $cpu.stack_pop(sys);
                    ByteRef::High(&mut $r).set(b);
                    $cpu.state = State::Fetch;
                }
                _ => unreachable!(),
            }
        }
    }
}

push!(pha, cpu, cpu.a, cpu.a16());
push!(phb, cpu, cpu.dbr as u16, false);
push!(phd, cpu, cpu.d, true);
push!(phk, cpu, cpu.pbr as u16, false);
push!(php, cpu, cpu.flags.as_byte() as u16, false);
push!(phx, cpu, cpu.x, cpu.m16());
push!(phy, cpu, cpu.y, cpu.m16());
pull16!(pla, cpu, cpu.a, cpu.a16());
pull8!(plb, cpu, b, cpu.dbr = b);
pull16!(pld, cpu, cpu.d, true);
pull8!(plp, cpu, p, cpu.set_p(p));
pull16!(plx, cpu, cpu.x, cpu.m16());
pull16!(ply, cpu, cpu.y, cpu.m16());

fn asl(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    let b16 = cpu.a16();
    let res = am.rwb(cpu, sys, |cpu, mut v, b16| if b16 {
        cpu.flags.carry = v >> 15 != 0;
        v <<= 1;
        cpu.flags.zero = v == 0;
        cpu.flags.negative = v >> 15 != 0;
        v
    } else {
        let mut v = v as u8;
        cpu.flags.carry = v >> 7 != 0;
        v <<= 1;
        cpu.flags.zero = v == 0;
        cpu.flags.negative = v >> 7 != 0;
        v as u16
    }, b16);

    if let Some(TaggedByte::Data(Byte::Low(_))) = res {
        cpu.state = State::Fetch;
    }
}

fn inc(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    let b16 = cpu.a16();
    let res = am.rwb(cpu, sys, |cpu, mut v, b16| if b16 {
        v = v.wrapping_add(1);
        cpu.flags.zero = v == 0;
        cpu.flags.negative = v >> 15 != 0;
        v
    } else {
        let mut v = v as u8;
        v = v.wrapping_add(1);
        cpu.flags.zero = v == 0;
        cpu.flags.negative = v >> 7 != 0;
        v as u16
    }, b16);

    if let Some(TaggedByte::Data(Byte::Low(_))) = res {
        cpu.state = State::Fetch;
    }
}

fn dec(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
    let b16 = cpu.a16();
    let res = am.rwb(cpu, sys, |cpu, mut v, b16| if b16 {
        v = v.wrapping_sub(1);
        cpu.flags.zero = v == 0;
        cpu.flags.negative = v >> 15 != 0;
        v
    } else {
        let mut v = v as u8;
        v = v.wrapping_sub(1);
        cpu.flags.zero = v == 0;
        cpu.flags.negative = v >> 7 != 0;
        v as u16
    }, b16);

    if let Some(TaggedByte::Data(Byte::Low(_))) = res {
        cpu.state = State::Fetch;
    }
}

macro_rules! push_effective {
    ($name:ident) => {
        fn $name(cpu: &mut CPU, sys: &mut dyn System, am: AddressingMode) {
            if cpu.tcu == 1 {
                cpu.temp_bank = 0;
            }

            match cpu.temp_bank {
                1 => {
                    cpu.stack_push(sys, (cpu.temp_data >> 8) as u8, false);
                    cpu.temp_bank += 1;
                }
                2 => {
                    cpu.stack_push(sys, cpu.temp_data as u8, false);
                    cpu.state = State::Fetch;
                }
                _ => match am.read(cpu, sys) {
                    Some(TaggedByte::Data(Byte::Low(l))) => cpu.temp_data = l as u16,
                    Some(TaggedByte::Data(Byte::High(h))) => {
                        cpu.temp_data |= (h as u16) << 8;
                        cpu.temp_bank = 1;
                    }
                    _ => (),
                }
            }
        }
    }
}

push_effective!(pea);
push_effective!(pei);

pub const INSTRUCTIONS: [(InstructionFn, AddressingMode); 0x100] = [
    (brk_cop, AddressingMode::Implied), // 00 (won't be implemented, this is directly in the state
                                        // machine as State::Brk)
    (ora, AddressingMode::DirectIndirectX), // 01
    (brk_cop, AddressingMode::Implied), // 02 (same as brk, this is COP)
    (ora, AddressingMode::StackRel), // 03
    (todo, AddressingMode::Implied), // 04
    (ora, AddressingMode::Direct), // 05
    (asl, AddressingMode::Direct), // 06
    (ora, AddressingMode::DirectIndirectLong), // 07
    (php, AddressingMode::Implied), // 08
    (ora, AddressingMode::Immediate), // 09
    (asl, AddressingMode::Accumulator), // 0a
    (phd, AddressingMode::Implied), // 0b
    (todo, AddressingMode::Implied), // 0c
    (ora, AddressingMode::Absolute), // 0d
    (asl, AddressingMode::Absolute), // 0e
    (ora, AddressingMode::AbsoluteLong), // 0f
    (bpl, AddressingMode::Immediate), // 10
    (ora, AddressingMode::DirectIndirectIndexedY), // 11
    (ora, AddressingMode::DirectIndirect), // 12
    (ora, AddressingMode::StackRelIndirectIndexedY), // 13
    (todo, AddressingMode::Implied), // 14
    (ora, AddressingMode::DirectIndexedX), // 15
    (asl, AddressingMode::DirectIndexedX), // 16
    (ora, AddressingMode::DirectIndirectLongIndexedY), // 17
    (clc, AddressingMode::Implied), // 18
    (ora, AddressingMode::AbsoluteIndexedY), // 19
    (inc, AddressingMode::Accumulator), // 1a
    (tcs, AddressingMode::Implied), // 1b
    (todo, AddressingMode::Implied), // 1c
    (ora, AddressingMode::AbsoluteIndexedX), // 1d
    (asl, AddressingMode::AbsoluteIndexedX), // 1e
    (ora, AddressingMode::AbsoluteLongIndexedX), // 1f
    (jsr, AddressingMode::Absolute), // 20
    (and, AddressingMode::DirectIndirectX), // 21
    (jsr_al, AddressingMode::AbsoluteLong), // 22
    (and, AddressingMode::StackRel), // 23
    (todo, AddressingMode::Implied), // 24
    (and, AddressingMode::Direct), // 25
    (todo, AddressingMode::Implied), // 26
    (and, AddressingMode::DirectIndirectLong), // 27
    (plp, AddressingMode::Implied), // 28
    (and, AddressingMode::Immediate), // 29
    (todo, AddressingMode::Implied), // 2a
    (pld, AddressingMode::Implied), // 2b
    (todo, AddressingMode::Implied), // 2c
    (and, AddressingMode::Absolute), // 2d
    (todo, AddressingMode::Implied), // 2e
    (and, AddressingMode::AbsoluteLong), // 2f
    (bmi, AddressingMode::Immediate), // 30
    (and, AddressingMode::DirectIndirectIndexedY), // 31
    (and, AddressingMode::DirectIndirect), // 32
    (and, AddressingMode::StackRelIndirectIndexedY), // 33
    (todo, AddressingMode::Implied), // 34
    (and, AddressingMode::DirectIndexedX), // 35
    (todo, AddressingMode::Implied), // 36
    (and, AddressingMode::DirectIndirectLongIndexedY), // 37
    (sec, AddressingMode::Implied), // 38
    (and, AddressingMode::AbsoluteIndexedY), // 39
    (dec, AddressingMode::Accumulator), // 3a
    (tsc, AddressingMode::Implied), // 3b
    (todo, AddressingMode::Implied), // 3c
    (and, AddressingMode::AbsoluteIndexedX), // 3d
    (todo, AddressingMode::Implied), // 3e
    (and, AddressingMode::AbsoluteLongIndexedX), // 3f
    (rti, AddressingMode::Implied), // 40
    (eor, AddressingMode::DirectIndirectX), // 41
    (todo, AddressingMode::Implied), // 42
    (eor, AddressingMode::StackRel), // 43
    (todo, AddressingMode::Implied), // 44
    (eor, AddressingMode::Direct), // 45
    (todo, AddressingMode::Implied), // 46
    (eor, AddressingMode::DirectIndirectLong), // 47
    (pha, AddressingMode::Implied), // 48
    (eor, AddressingMode::Immediate), // 49
    (todo, AddressingMode::Implied), // 4a
    (phk, AddressingMode::Implied), // 4b
    (jmp, AddressingMode::Immediate), // 4c
    (eor, AddressingMode::Absolute), // 4d
    (todo, AddressingMode::Implied), // 4e
    (eor, AddressingMode::AbsoluteLong), // 4f
    (bvc, AddressingMode::Immediate), // 50
    (eor, AddressingMode::DirectIndirectIndexedY), // 51
    (eor, AddressingMode::DirectIndirect), // 52
    (eor, AddressingMode::StackRelIndirectIndexedY), // 53
    (todo, AddressingMode::Implied), // 54
    (eor, AddressingMode::DirectIndexedX), // 55
    (todo, AddressingMode::Implied), // 56
    (eor, AddressingMode::DirectIndirectLongIndexedY), // 57
    (cli, AddressingMode::Implied), // 58
    (eor, AddressingMode::AbsoluteIndexedY), // 59
    (phy, AddressingMode::Implied), // 5a
    (tcd, AddressingMode::Implied), // 5b
    (jml, AddressingMode::AbsoluteLong), // 5c
    (eor, AddressingMode::AbsoluteIndexedX), // 5d
    (todo, AddressingMode::Implied), // 5e
    (eor, AddressingMode::AbsoluteLongIndexedX), // 5f
    (rts, AddressingMode::Absolute), // 60
    (adc, AddressingMode::DirectIndirectX), // 61
    (todo, AddressingMode::Implied), // 62
    (adc, AddressingMode::StackRel), // 63
    (stz, AddressingMode::Direct), // 64
    (adc, AddressingMode::Direct), // 65
    (todo, AddressingMode::Implied), // 66
    (adc, AddressingMode::DirectIndirectLong), // 67
    (pla, AddressingMode::Implied), // 68
    (adc, AddressingMode::Immediate), // 69
    (todo, AddressingMode::Implied), // 6a
    (rts, AddressingMode::AbsoluteLong), // 6b
    (jmp, AddressingMode::Indirect), // 6c
    (adc, AddressingMode::Absolute), // 6d
    (todo, AddressingMode::Implied), // 6e
    (adc, AddressingMode::AbsoluteLong), // 6f
    (bvs, AddressingMode::Immediate), // 70
    (adc, AddressingMode::DirectIndirectIndexedY), // 71
    (adc, AddressingMode::DirectIndirect), // 72
    (adc, AddressingMode::StackRelIndirectIndexedY), // 73
    (stz, AddressingMode::DirectIndexedX), // 74
    (adc, AddressingMode::DirectIndexedX), // 75
    (todo, AddressingMode::Implied), // 76
    (adc, AddressingMode::DirectIndirectLongIndexedY), // 77
    (sei, AddressingMode::Implied), // 78
    (adc, AddressingMode::AbsoluteIndexedY), // 79
    (ply, AddressingMode::Implied), // 7a
    (tdc, AddressingMode::Implied), // 7b
    (jmp, AddressingMode::IndexedIndirectX), // 7c
    (adc, AddressingMode::AbsoluteIndexedX), // 7d
    (todo, AddressingMode::Implied), // 7e
    (adc, AddressingMode::AbsoluteLongIndexedX), // 7f
    (bra, AddressingMode::Immediate), // 80
    (sta, AddressingMode::DirectIndirectX), // 81
    (todo, AddressingMode::Implied), // 82
    (sta, AddressingMode::StackRel), // 83
    (sty, AddressingMode::Direct), // 84
    (sta, AddressingMode::Direct), // 85
    (stx, AddressingMode::Direct), // 86
    (sta, AddressingMode::DirectIndirectLong), // 87
    (todo, AddressingMode::Implied), // 88
    (todo, AddressingMode::Implied), // 89
    (txa, AddressingMode::Implied), // 8a
    (phb, AddressingMode::Implied), // 8b
    (sty, AddressingMode::Absolute), // 8c
    (sta, AddressingMode::Absolute), // 8d
    (stx, AddressingMode::Absolute), // 8e
    (sta, AddressingMode::AbsoluteLong), // 8f
    (bcc, AddressingMode::Immediate), // 90
    (sta, AddressingMode::DirectIndirectIndexedY), // 91
    (sta, AddressingMode::DirectIndirect), // 92
    (sta, AddressingMode::StackRelIndirectIndexedY), // 93
    (sty, AddressingMode::DirectIndexedX), // 94
    (sta, AddressingMode::DirectIndexedX), // 95
    (stx, AddressingMode::DirectIndexedY), // 96
    (sta, AddressingMode::DirectIndirectLongIndexedY), // 97
    (tya, AddressingMode::Implied), // 98
    (sta, AddressingMode::AbsoluteIndexedY), // 99
    (txs, AddressingMode::Implied), // 9a
    (txy, AddressingMode::Implied), // 9b
    (stz, AddressingMode::Absolute), // 9c
    (sta, AddressingMode::AbsoluteIndexedX), // 9d
    (stz, AddressingMode::AbsoluteIndexedX), // 9e
    (sta, AddressingMode::AbsoluteLongIndexedX), // 9f
    (ldy, AddressingMode::Immediate), // a0
    (lda, AddressingMode::DirectIndirectX), // a1
    (ldx, AddressingMode::Immediate), // a2
    (lda, AddressingMode::StackRel), // a3
    (ldy, AddressingMode::Direct), // a4
    (lda, AddressingMode::Direct), // a5
    (ldx, AddressingMode::Direct), // a6
    (lda, AddressingMode::DirectIndirectLong), // a7
    (tay, AddressingMode::Implied), // a8
    (lda, AddressingMode::Immediate), // a9
    (tax, AddressingMode::Implied), // aa
    (plb, AddressingMode::Implied), // ab
    (ldy, AddressingMode::Absolute), // ac
    (lda, AddressingMode::Absolute), // ad
    (ldx, AddressingMode::Absolute), // ae
    (lda, AddressingMode::AbsoluteLong), // af
    (bcs, AddressingMode::Immediate), // b0
    (lda, AddressingMode::DirectIndirectIndexedY), // b1
    (lda, AddressingMode::DirectIndirect), // b2
    (lda, AddressingMode::StackRelIndirectIndexedY), // b3
    (ldy, AddressingMode::DirectIndexedX), // b4
    (lda, AddressingMode::DirectIndexedX), // b5
    (ldx, AddressingMode::DirectIndexedY), // b6
    (lda, AddressingMode::DirectIndirectLongIndexedY), // b7
    (clv, AddressingMode::Implied), // b8
    (lda, AddressingMode::AbsoluteIndexedY), // b9
    (tsx, AddressingMode::Implied), // ba
    (tyx, AddressingMode::Implied), // bb
    (ldy, AddressingMode::AbsoluteIndexedX), // bc
    (lda, AddressingMode::AbsoluteIndexedX), // bd
    (ldx, AddressingMode::AbsoluteIndexedY), // be
    (lda, AddressingMode::AbsoluteLongIndexedX), // bf
    (todo, AddressingMode::Implied), // c0
    (todo, AddressingMode::Implied), // c1
    (rep, AddressingMode::Immediate), // c2
    (todo, AddressingMode::Implied), // c3
    (todo, AddressingMode::Implied), // c4
    (todo, AddressingMode::Implied), // c5
    (dec, AddressingMode::Direct), // c6
    (todo, AddressingMode::Implied), // c7
    (todo, AddressingMode::Implied), // c8
    (todo, AddressingMode::Implied), // c9
    (todo, AddressingMode::Implied), // ca
    (wai, AddressingMode::Implied), // cb
    (todo, AddressingMode::Implied), // cc
    (todo, AddressingMode::Implied), // cd
    (dec, AddressingMode::Absolute), // ce
    (todo, AddressingMode::Implied), // cf
    (bne, AddressingMode::Immediate), // d0
    (todo, AddressingMode::Implied), // d1
    (todo, AddressingMode::Implied), // d2
    (todo, AddressingMode::Implied), // d3
    (pei, AddressingMode::Direct), // d4
    (todo, AddressingMode::Implied), // d5
    (dec, AddressingMode::DirectIndexedX), // d6
    (todo, AddressingMode::Implied), // d7
    (cld, AddressingMode::Implied), // d8
    (todo, AddressingMode::Implied), // d9
    (phx, AddressingMode::Implied), // da
    (stp, AddressingMode::Implied), // db
    (jmp, AddressingMode::IndirectLong), // dc
    (todo, AddressingMode::Implied), // dd
    (dec, AddressingMode::AbsoluteIndexedX), // de
    (todo, AddressingMode::Implied), // df
    (todo, AddressingMode::Implied), // e0
    (sbc, AddressingMode::DirectIndirectX), // e1
    (sep, AddressingMode::Immediate), // e2
    (sbc, AddressingMode::StackRel), // e3
    (todo, AddressingMode::Implied), // e4
    (sbc, AddressingMode::Direct), // e5
    (inc, AddressingMode::Direct), // e6
    (sbc, AddressingMode::DirectIndirectLong), // e7
    (todo, AddressingMode::Implied), // e8
    (sbc, AddressingMode::Immediate), // e9
    (nop, AddressingMode::Implied), // ea
    (todo, AddressingMode::Implied), // eb
    (todo, AddressingMode::Implied), // ec
    (sbc, AddressingMode::Absolute), // ed
    (inc, AddressingMode::Absolute), // ee
    (sbc, AddressingMode::AbsoluteLong), // ef
    (beq, AddressingMode::Immediate), // f0
    (sbc, AddressingMode::DirectIndirectIndexedY), // f1
    (sbc, AddressingMode::DirectIndirect), // f2
    (sbc, AddressingMode::StackRelIndirectIndexedY), // f3
    (pea, AddressingMode::Immediate), // f4
    (sbc, AddressingMode::DirectIndexedX), // f5
    (inc, AddressingMode::DirectIndexedX), // f6
    (sbc, AddressingMode::DirectIndirectLongIndexedY), // f7
    (sed, AddressingMode::Implied), // f8
    (sbc, AddressingMode::AbsoluteIndexedY), // f9
    (plx, AddressingMode::Implied), // fa
    (xce, AddressingMode::Implied), // fb
    (jsr, AddressingMode::IndexedIndirectX), // fc
    (sbc, AddressingMode::AbsoluteIndexedX), // fd
    (inc, AddressingMode::AbsoluteIndexedX), // fe
    (sbc, AddressingMode::AbsoluteLongIndexedX), // ff
];
