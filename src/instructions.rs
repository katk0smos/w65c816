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
fn brk(_cpu: &mut CPU, _sys: &mut dyn System, _am: AddressingMode) {}

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
            let pc = ((cpu.pbr as u32) << 16) | ((cpu.temp_addr+cpu.x) as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::Low(&mut cpu.temp_data).set(data);
        }
        (7, IDX_IN) => {
            // pbr,aa+x+1
            let pc = ((cpu.pbr as u32) << 16) | ((1+cpu.temp_addr+cpu.x) as u32);
            let data = sys.read(pc, AddressType::Program, &cpu.signals);
            ByteRef::High(&mut cpu.temp_data).set(data);
            cpu.pc = cpu.temp_data;
            cpu.state = State::Fetch;
        }
        _ => unreachable!(),
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

    if cpu.tcu == 2 {
        if !cpu.aborted {
            cpu.flags.set_mask(data, true);
            cpu.signals.m = cpu.flags.mem_sel;
            cpu.signals.x = cpu.flags.index_sel;
            if cpu.flags.index_sel {
                ByteRef::High(&mut cpu.x).set(0);
                ByteRef::High(&mut cpu.y).set(0);
            }
        }
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
            let mut s = cpu.s.wrapping_add(1);
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
            cpu.pc = cpu.pc.wrapping_add(1);
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
            if $cpu.tcu == 2 {
                if !$cpu.aborted {
                    $r1 = $r0;
                }
            }
            $cpu.state = State::Fetch;
        }
    }
}

transfer16!(tcd, cpu, cpu.a, cpu.d);
transfer16!(tcs, cpu, cpu.a, cpu.s);
transfer16!(tdc, cpu, cpu.d, cpu.a);
transfer16!(tsc, cpu, cpu.s, cpu.a);

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
branch!(beq, cpu, cpu.flags.carry);
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
                1 => {
                    let b = ByteRef::Low(&mut r).get();
                    $cpu.stack_push(sys, b, false);

                    if !($b16) {
                        $cpu.state = State::Fetch;
                    }
                }
                2 => {
                    let b = ByteRef::High(&mut r).get();
                    $cpu.stack_push(sys, b, false);
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

pub const INSTRUCTIONS: [(InstructionFn, AddressingMode); 0x100] = [
    (brk, AddressingMode::Implied), // 00 (won't be implemented, this is directly in the state
                                     // machine as State::Brk)
    (todo, AddressingMode::Implied), // 01
    (todo, AddressingMode::Implied), // 02
    (todo, AddressingMode::Implied), // 03
    (todo, AddressingMode::Implied), // 04
    (todo, AddressingMode::Implied), // 05
    (todo, AddressingMode::Implied), // 06
    (todo, AddressingMode::Implied), // 07
    (php, AddressingMode::Implied), // 08
    (todo, AddressingMode::Implied), // 09
    (todo, AddressingMode::Implied), // 0a
    (phd, AddressingMode::Implied), // 0b
    (todo, AddressingMode::Implied), // 0c
    (todo, AddressingMode::Implied), // 0d
    (todo, AddressingMode::Implied), // 0e
    (todo, AddressingMode::Implied), // 0f
    (bpl, AddressingMode::Immediate), // 10
    (todo, AddressingMode::Implied), // 11
    (todo, AddressingMode::Implied), // 12
    (todo, AddressingMode::Implied), // 13
    (todo, AddressingMode::Implied), // 14
    (todo, AddressingMode::Implied), // 15
    (todo, AddressingMode::Implied), // 16
    (todo, AddressingMode::Implied), // 17
    (clc, AddressingMode::Implied), // 18
    (todo, AddressingMode::Implied), // 19
    (todo, AddressingMode::Implied), // 1a
    (tcs, AddressingMode::Implied), // 1b
    (todo, AddressingMode::Implied), // 1c
    (todo, AddressingMode::Implied), // 1d
    (todo, AddressingMode::Implied), // 1e
    (todo, AddressingMode::Implied), // 1f
    (jsr, AddressingMode::Absolute), // 20
    (and, AddressingMode::DirectIndirectX), // 21
    (jsr_al, AddressingMode::AbsoluteLong), // 22
    (and, AddressingMode::StackRel), // 23
    (todo, AddressingMode::Implied), // 24
    (and, AddressingMode::Direct), // 25
    (todo, AddressingMode::Implied), // 26
    (and, AddressingMode::DirectIndirectLong), // 27
    (todo, AddressingMode::Implied), // 28
    (and, AddressingMode::Immediate), // 29
    (todo, AddressingMode::Implied), // 2a
    (todo, AddressingMode::Implied), // 2b
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
    (todo, AddressingMode::Implied), // 3a
    (tsc, AddressingMode::Implied), // 3b
    (todo, AddressingMode::Implied), // 3c
    (and, AddressingMode::AbsoluteIndexedX), // 3d
    (todo, AddressingMode::Implied), // 3e
    (and, AddressingMode::AbsoluteLongIndexedX), // 3f
    (rti, AddressingMode::Implied), // 40
    (todo, AddressingMode::Implied), // 41
    (todo, AddressingMode::Implied), // 42
    (todo, AddressingMode::Implied), // 43
    (todo, AddressingMode::Implied), // 44
    (todo, AddressingMode::Implied), // 45
    (todo, AddressingMode::Implied), // 46
    (todo, AddressingMode::Implied), // 47
    (pha, AddressingMode::Implied), // 48
    (todo, AddressingMode::Implied), // 49
    (todo, AddressingMode::Implied), // 4a
    (phk, AddressingMode::Implied), // 4b
    (todo, AddressingMode::Implied), // 4c
    (todo, AddressingMode::Implied), // 4d
    (todo, AddressingMode::Implied), // 4e
    (todo, AddressingMode::Implied), // 4f
    (bvc, AddressingMode::Immediate), // 50
    (todo, AddressingMode::Implied), // 51
    (todo, AddressingMode::Implied), // 52
    (todo, AddressingMode::Implied), // 53
    (todo, AddressingMode::Implied), // 54
    (todo, AddressingMode::Implied), // 55
    (todo, AddressingMode::Implied), // 56
    (todo, AddressingMode::Implied), // 57
    (cli, AddressingMode::Implied), // 58
    (todo, AddressingMode::Implied), // 59
    (phy, AddressingMode::Implied), // 5a
    (tcd, AddressingMode::Implied), // 5b
    (todo, AddressingMode::Implied), // 5c
    (todo, AddressingMode::Implied), // 5d
    (todo, AddressingMode::Implied), // 5e
    (todo, AddressingMode::Implied), // 5f
    (rts, AddressingMode::Absolute), // 60
    (todo, AddressingMode::Implied), // 61
    (todo, AddressingMode::Implied), // 62
    (todo, AddressingMode::Implied), // 63
    (stz, AddressingMode::Direct), // 64
    (todo, AddressingMode::Implied), // 65
    (todo, AddressingMode::Implied), // 66
    (todo, AddressingMode::Implied), // 67
    (todo, AddressingMode::Implied), // 68
    (todo, AddressingMode::Implied), // 69
    (todo, AddressingMode::Implied), // 6a
    (rts, AddressingMode::AbsoluteLong), // 6b
    (todo, AddressingMode::Implied), // 6c
    (todo, AddressingMode::Implied), // 6d
    (todo, AddressingMode::Implied), // 6e
    (todo, AddressingMode::Implied), // 6f
    (bvs, AddressingMode::Immediate), // 70
    (todo, AddressingMode::Implied), // 71
    (todo, AddressingMode::Implied), // 72
    (todo, AddressingMode::Implied), // 73
    (stz, AddressingMode::DirectIndexedX), // 74
    (todo, AddressingMode::Implied), // 75
    (todo, AddressingMode::Implied), // 76
    (todo, AddressingMode::Implied), // 77
    (sei, AddressingMode::Implied), // 78
    (todo, AddressingMode::Implied), // 79
    (todo, AddressingMode::Implied), // 7a
    (tdc, AddressingMode::Implied), // 7b
    (todo, AddressingMode::Implied), // 7c
    (todo, AddressingMode::Implied), // 7d
    (todo, AddressingMode::Implied), // 7e
    (todo, AddressingMode::Implied), // 7f
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
    (todo, AddressingMode::Implied), // ab
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
    (todo, AddressingMode::Implied), // c2
    (todo, AddressingMode::Implied), // c3
    (todo, AddressingMode::Implied), // c4
    (todo, AddressingMode::Implied), // c5
    (todo, AddressingMode::Implied), // c6
    (todo, AddressingMode::Implied), // c7
    (todo, AddressingMode::Implied), // c8
    (todo, AddressingMode::Implied), // c9
    (todo, AddressingMode::Implied), // ca
    (wai, AddressingMode::Implied), // cb
    (todo, AddressingMode::Implied), // cc
    (todo, AddressingMode::Implied), // cd
    (todo, AddressingMode::Implied), // ce
    (todo, AddressingMode::Implied), // cf
    (bne, AddressingMode::Immediate), // d0
    (todo, AddressingMode::Implied), // d1
    (todo, AddressingMode::Implied), // d2
    (todo, AddressingMode::Implied), // d3
    (todo, AddressingMode::Implied), // d4
    (todo, AddressingMode::Implied), // d5
    (todo, AddressingMode::Implied), // d6
    (todo, AddressingMode::Implied), // d7
    (cld, AddressingMode::Implied), // d8
    (todo, AddressingMode::Implied), // d9
    (phx, AddressingMode::Implied), // da
    (stp, AddressingMode::Implied), // db
    (todo, AddressingMode::Implied), // dc
    (todo, AddressingMode::Implied), // dd
    (todo, AddressingMode::Implied), // de
    (todo, AddressingMode::Implied), // df
    (todo, AddressingMode::Implied), // e0
    (todo, AddressingMode::Implied), // e1
    (sep, AddressingMode::Immediate), // e2
    (todo, AddressingMode::Implied), // e3
    (todo, AddressingMode::Implied), // e4
    (todo, AddressingMode::Implied), // e5
    (todo, AddressingMode::Implied), // e6
    (todo, AddressingMode::Implied), // e7
    (todo, AddressingMode::Implied), // e8
    (todo, AddressingMode::Implied), // e9
    (nop, AddressingMode::Implied), // ea
    (todo, AddressingMode::Implied), // eb
    (todo, AddressingMode::Implied), // ec
    (todo, AddressingMode::Implied), // ed
    (todo, AddressingMode::Implied), // ee
    (todo, AddressingMode::Implied), // ef
    (beq, AddressingMode::Immediate), // f0
    (todo, AddressingMode::Implied), // f1
    (todo, AddressingMode::Implied), // f2
    (todo, AddressingMode::Implied), // f3
    (todo, AddressingMode::Implied), // f4
    (todo, AddressingMode::Implied), // f5
    (todo, AddressingMode::Implied), // f6
    (todo, AddressingMode::Implied), // f7
    (sed, AddressingMode::Implied), // f8
    (todo, AddressingMode::Implied), // f9
    (todo, AddressingMode::Implied), // fa
    (xce, AddressingMode::Implied), // fb
    (jsr, AddressingMode::IndexedIndirectX), // fc
    (todo, AddressingMode::Implied), // fd
    (todo, AddressingMode::Implied), // fe
    (todo, AddressingMode::Implied), // ff
];
