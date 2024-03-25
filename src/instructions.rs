use super::{CPU, System, AddressType, AddressingMode, State, ByteRef, Byte, TaggedByte};

pub type InstructionFn = fn(&mut CPU, &mut dyn System, AddressingMode) -> ();

fn todo(cpu: &mut CPU, _sys: &mut dyn System, _am: AddressingMode) {
    todo!("{:02x}", cpu.ir);
}

#[inline(always)]
fn io(cpu: &mut CPU, sys: &mut dyn System) {
    let effective = ((cpu.pbr as u32) << 16) | (cpu.pc as u32);
    let _ = sys.read(effective, AddressType::Invalid, &cpu.signals);
}

#[inline]
fn implied(cpu: &mut CPU, sys: &mut dyn System) {
    AddressingMode::Implied.read(cpu, sys);

    if cpu.tcu == 1 {
        cpu.state = State::Fetch;
    }
}

fn nop(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
}

fn clc(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    cpu.flags.carry = false;
}

fn sec(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    cpu.flags.carry = true;
}

fn cli(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    cpu.flags.interrupt_disable = false;
}

fn sei(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    cpu.flags.interrupt_disable = true;
}

fn cld(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    cpu.flags.decimal = false;
}

fn sed(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    cpu.flags.decimal = true;
}

fn clv(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    cpu.flags.overflow = false;
}

fn xce(cpu: &mut CPU, sys: &mut dyn System, _am: AddressingMode) {
    implied(cpu, sys);
    if cpu.tcu == 1 {
        let old_e = cpu.flags.emulation;
        cpu.flags.emulation = cpu.flags.carry;
        cpu.flags.carry = old_e;
        cpu.set_e(cpu.flags.emulation);
    }
}

#[inline]
fn jsr_al(cpu: &mut CPU, sys: &mut dyn System) {
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
    if am == AddressingMode::AbsoluteLong {
        return jsr_al(cpu, sys);
    }

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
        cpu.flags.set_mask(data, true);
        cpu.signals.m = cpu.flags.mem_sel;
        cpu.signals.x = cpu.flags.index_sel;
        if cpu.flags.index_sel {
            ByteRef::High(&mut cpu.x).set(0);
            ByteRef::High(&mut cpu.y).set(0);
        }
        cpu.state = State::Fetch;
    }
}

pub const INSTRUCTIONS: [(InstructionFn, AddressingMode); 0x100] = [
    (todo, AddressingMode::Implied), // 00
    (todo, AddressingMode::Implied), // 01
    (todo, AddressingMode::Implied), // 02
    (todo, AddressingMode::Implied), // 03
    (todo, AddressingMode::Implied), // 04
    (todo, AddressingMode::Implied), // 05
    (todo, AddressingMode::Implied), // 06
    (todo, AddressingMode::Implied), // 07
    (todo, AddressingMode::Implied), // 08
    (todo, AddressingMode::Implied), // 09
    (todo, AddressingMode::Implied), // 0a
    (todo, AddressingMode::Implied), // 0b
    (todo, AddressingMode::Implied), // 0c
    (todo, AddressingMode::Implied), // 0d
    (todo, AddressingMode::Implied), // 0e
    (todo, AddressingMode::Implied), // 0f
    (todo, AddressingMode::Implied), // 10
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
    (todo, AddressingMode::Implied), // 1b
    (todo, AddressingMode::Implied), // 1c
    (todo, AddressingMode::Implied), // 1d
    (todo, AddressingMode::Implied), // 1e
    (todo, AddressingMode::Implied), // 1f
    (jsr, AddressingMode::Absolute), // 20
    (todo, AddressingMode::Implied), // 21
    (jsr, AddressingMode::AbsoluteLong), // 22
    (todo, AddressingMode::Implied), // 23
    (todo, AddressingMode::Implied), // 24
    (todo, AddressingMode::Implied), // 25
    (todo, AddressingMode::Implied), // 26
    (todo, AddressingMode::Implied), // 27
    (todo, AddressingMode::Implied), // 28
    (todo, AddressingMode::Implied), // 29
    (todo, AddressingMode::Implied), // 2a
    (todo, AddressingMode::Implied), // 2b
    (todo, AddressingMode::Implied), // 2c
    (todo, AddressingMode::Implied), // 2d
    (todo, AddressingMode::Implied), // 2e
    (todo, AddressingMode::Implied), // 2f
    (todo, AddressingMode::Implied), // 30
    (todo, AddressingMode::Implied), // 31
    (todo, AddressingMode::Implied), // 32
    (todo, AddressingMode::Implied), // 33
    (todo, AddressingMode::Implied), // 34
    (todo, AddressingMode::Implied), // 35
    (todo, AddressingMode::Implied), // 36
    (todo, AddressingMode::Implied), // 37
    (sec, AddressingMode::Implied), // 38
    (todo, AddressingMode::Implied), // 39
    (todo, AddressingMode::Implied), // 3a
    (todo, AddressingMode::Implied), // 3b
    (todo, AddressingMode::Implied), // 3c
    (todo, AddressingMode::Implied), // 3d
    (todo, AddressingMode::Implied), // 3e
    (todo, AddressingMode::Implied), // 3f
    (todo, AddressingMode::Implied), // 40
    (todo, AddressingMode::Implied), // 41
    (todo, AddressingMode::Implied), // 42
    (todo, AddressingMode::Implied), // 43
    (todo, AddressingMode::Implied), // 44
    (todo, AddressingMode::Implied), // 45
    (todo, AddressingMode::Implied), // 46
    (todo, AddressingMode::Implied), // 47
    (todo, AddressingMode::Implied), // 48
    (todo, AddressingMode::Implied), // 49
    (todo, AddressingMode::Implied), // 4a
    (todo, AddressingMode::Implied), // 4b
    (todo, AddressingMode::Implied), // 4c
    (todo, AddressingMode::Implied), // 4d
    (todo, AddressingMode::Implied), // 4e
    (todo, AddressingMode::Implied), // 4f
    (todo, AddressingMode::Implied), // 50
    (todo, AddressingMode::Implied), // 51
    (todo, AddressingMode::Implied), // 52
    (todo, AddressingMode::Implied), // 53
    (todo, AddressingMode::Implied), // 54
    (todo, AddressingMode::Implied), // 55
    (todo, AddressingMode::Implied), // 56
    (todo, AddressingMode::Implied), // 57
    (cli, AddressingMode::Implied), // 58
    (todo, AddressingMode::Implied), // 59
    (todo, AddressingMode::Implied), // 5a
    (todo, AddressingMode::Implied), // 5b
    (todo, AddressingMode::Implied), // 5c
    (todo, AddressingMode::Implied), // 5d
    (todo, AddressingMode::Implied), // 5e
    (todo, AddressingMode::Implied), // 5f
    (todo, AddressingMode::Implied), // 60
    (todo, AddressingMode::Implied), // 61
    (todo, AddressingMode::Implied), // 62
    (todo, AddressingMode::Implied), // 63
    (todo, AddressingMode::Implied), // 64
    (todo, AddressingMode::Implied), // 65
    (todo, AddressingMode::Implied), // 66
    (todo, AddressingMode::Implied), // 67
    (todo, AddressingMode::Implied), // 68
    (todo, AddressingMode::Implied), // 69
    (todo, AddressingMode::Implied), // 6a
    (todo, AddressingMode::Implied), // 6b
    (todo, AddressingMode::Implied), // 6c
    (todo, AddressingMode::Implied), // 6d
    (todo, AddressingMode::Implied), // 6e
    (todo, AddressingMode::Implied), // 6f
    (todo, AddressingMode::Implied), // 70
    (todo, AddressingMode::Implied), // 71
    (todo, AddressingMode::Implied), // 72
    (todo, AddressingMode::Implied), // 73
    (todo, AddressingMode::Implied), // 74
    (todo, AddressingMode::Implied), // 75
    (todo, AddressingMode::Implied), // 76
    (todo, AddressingMode::Implied), // 77
    (sei, AddressingMode::Implied), // 78
    (todo, AddressingMode::Implied), // 79
    (todo, AddressingMode::Implied), // 7a
    (todo, AddressingMode::Implied), // 7b
    (todo, AddressingMode::Implied), // 7c
    (todo, AddressingMode::Implied), // 7d
    (todo, AddressingMode::Implied), // 7e
    (todo, AddressingMode::Implied), // 7f
    (todo, AddressingMode::Implied), // 80
    (todo, AddressingMode::Implied), // 81
    (todo, AddressingMode::Implied), // 82
    (todo, AddressingMode::Implied), // 83
    (todo, AddressingMode::Implied), // 84
    (todo, AddressingMode::Implied), // 85
    (todo, AddressingMode::Implied), // 86
    (todo, AddressingMode::Implied), // 87
    (todo, AddressingMode::Implied), // 88
    (todo, AddressingMode::Implied), // 89
    (todo, AddressingMode::Implied), // 8a
    (todo, AddressingMode::Implied), // 8b
    (todo, AddressingMode::Implied), // 8c
    (todo, AddressingMode::Implied), // 8d
    (todo, AddressingMode::Implied), // 8e
    (todo, AddressingMode::Implied), // 8f
    (todo, AddressingMode::Implied), // 90
    (todo, AddressingMode::Implied), // 91
    (todo, AddressingMode::Implied), // 92
    (todo, AddressingMode::Implied), // 93
    (todo, AddressingMode::Implied), // 94
    (todo, AddressingMode::Implied), // 95
    (todo, AddressingMode::Implied), // 96
    (todo, AddressingMode::Implied), // 97
    (todo, AddressingMode::Implied), // 98
    (todo, AddressingMode::Implied), // 99
    (todo, AddressingMode::Implied), // 9a
    (todo, AddressingMode::Implied), // 9b
    (todo, AddressingMode::Implied), // 9c
    (todo, AddressingMode::Implied), // 9d
    (todo, AddressingMode::Implied), // 9e
    (todo, AddressingMode::Implied), // 9f
    (todo, AddressingMode::Implied), // a0
    (todo, AddressingMode::Implied), // a1
    (todo, AddressingMode::Implied), // a2
    (todo, AddressingMode::Implied), // a3
    (todo, AddressingMode::Implied), // a4
    (todo, AddressingMode::Implied), // a5
    (todo, AddressingMode::Implied), // a6
    (todo, AddressingMode::Implied), // a7
    (todo, AddressingMode::Implied), // a8
    (todo, AddressingMode::Implied), // a9
    (todo, AddressingMode::Implied), // aa
    (todo, AddressingMode::Implied), // ab
    (todo, AddressingMode::Implied), // ac
    (todo, AddressingMode::Implied), // ad
    (todo, AddressingMode::Implied), // ae
    (todo, AddressingMode::Implied), // af
    (todo, AddressingMode::Implied), // b0
    (todo, AddressingMode::Implied), // b1
    (todo, AddressingMode::Implied), // b2
    (todo, AddressingMode::Implied), // b3
    (todo, AddressingMode::Implied), // b4
    (todo, AddressingMode::Implied), // b5
    (todo, AddressingMode::Implied), // b6
    (todo, AddressingMode::Implied), // b7
    (clv, AddressingMode::Implied), // b8
    (todo, AddressingMode::Implied), // b9
    (todo, AddressingMode::Implied), // ba
    (todo, AddressingMode::Implied), // bb
    (todo, AddressingMode::Implied), // bc
    (todo, AddressingMode::Implied), // bd
    (todo, AddressingMode::Implied), // be
    (todo, AddressingMode::Implied), // bf
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
    (todo, AddressingMode::Implied), // cb
    (todo, AddressingMode::Implied), // cc
    (todo, AddressingMode::Implied), // cd
    (todo, AddressingMode::Implied), // ce
    (todo, AddressingMode::Implied), // cf
    (todo, AddressingMode::Implied), // d0
    (todo, AddressingMode::Implied), // d1
    (todo, AddressingMode::Implied), // d2
    (todo, AddressingMode::Implied), // d3
    (todo, AddressingMode::Implied), // d4
    (todo, AddressingMode::Implied), // d5
    (todo, AddressingMode::Implied), // d6
    (todo, AddressingMode::Implied), // d7
    (cld, AddressingMode::Implied), // d8
    (todo, AddressingMode::Implied), // d9
    (todo, AddressingMode::Implied), // da
    (todo, AddressingMode::Implied), // db
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
    (todo, AddressingMode::Implied), // f0
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
