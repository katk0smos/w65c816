use super::*;

struct Sys {
    ram: [u8; 0x10000],
    res: bool,
}

impl Default for Sys {
    fn default() -> Self {
        Self {
            ram: [0xEA; 0x10000],
            res: false,
        }
    }
}

impl System for Sys {
    fn read(&mut self, a: u32, at: AddressType, signals: &Signals) -> u8 {
        self.ram[(a & 0x00ffff) as usize]
    }

    fn write(&mut self, a: u32, d: u8, signals: &Signals) {
        self.ram[(a & 0xffffff) as usize] = d;
    }

    fn res(&mut self) -> bool {
        let x = self.res;

        if !x {
            self.res = true;
        }

        return !x;
    }
}

#[test]
fn reset() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;

    for _ in 0..8 {
        cpu.cycle(&mut sys);
    }

    assert_eq!(cpu.pc, 0x8000, "CPU reset improperly");
    assert_eq!(cpu.dbr, 00, "dbr");
    assert_eq!(cpu.pbr, 00, "pbr");
    assert_eq!(cpu.d, 0x0000, "d");
    assert_eq!(cpu.s & 0xff00, 0x0100, "s");
    assert_eq!(cpu.x & 0xff00, 0x0000, "x");
    assert_eq!(cpu.y & 0xff00, 0x0000, "y");
    assert_eq!(cpu.signals.e, cpu.flags.emulation, "emulation");
    assert_eq!(cpu.signals.e, true, "emulation");
    assert_eq!(cpu.signals.mx(false), true, "mx:m");
    assert_eq!(cpu.signals.mx(true), true, "mx:x");
    assert_eq!(cpu.flags.mem_sel, true, "m");
    assert_eq!(cpu.flags.index_sel, true, "x");
    assert_eq!(cpu.flags.decimal, false, "d");
    assert_eq!(cpu.flags.interrupt_disable, true, "i");
    assert_eq!(cpu.flags.carry, true, "c");
}

#[test]
fn init() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x8000] = 0xA9;
    sys.ram[0x8001] = 0x84;
    sys.ram[0x8002] = 0xA0;
    sys.ram[0x8003] = 0x95;
    sys.ram[0x8004] = 0xA2;
    sys.ram[0x8005] = 0x23;

    for _ in 0..8 + 2 * 3 {
        cpu.cycle(&mut sys);
    }

    assert_eq!(cpu.pc, 0x8006, "CPU reset improperly");
    assert_eq!(cpu.dbr, 00, "dbr");
    assert_eq!(cpu.pbr, 00, "pbr");
    assert_eq!(cpu.d, 0x0000, "d");
    assert_eq!(cpu.s & 0xff00, 0x0100, "s");
    assert_eq!(cpu.a & 0x00ff, 0x0084, "a");
    assert_eq!(cpu.x & 0xffff, 0x0023, "x");
    assert_eq!(cpu.y & 0xffff, 0x0095, "y");
    assert!(cpu.signals.e && cpu.flags.emulation, "emulation");
    assert_eq!(cpu.signals.e, true, "emulation");
    assert_eq!(cpu.signals.mx(false), true, "mx:m");
    assert_eq!(cpu.signals.mx(true), true, "mx:x");
    assert_eq!(cpu.flags.mem_sel, true, "m");
    assert_eq!(cpu.flags.index_sel, true, "x");
    assert_eq!(cpu.flags.decimal, false, "d");
    assert_eq!(cpu.flags.interrupt_disable, true, "i");
    assert_eq!(cpu.flags.carry, true, "c");
    assert_eq!(cpu.flags.zero, false, "z");
    assert_eq!(cpu.flags.negative, false, "n");
}

#[test]
fn runs_forever_with_nop() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();

    for _ in 0..8 + 0x20000 {
        cpu.cycle(&mut sys);
    }

    assert_eq!(cpu.pc, 0xEAEA);
    assert_eq!(cpu.dbr, 00, "dbr");
    assert_eq!(cpu.pbr, 00, "pbr");
    assert_eq!(cpu.d, 0x0000, "d");
    assert_eq!(cpu.s & 0xff00, 0x0100, "s");
    assert_eq!(cpu.x & 0xff00, 0x0000, "x");
    assert_eq!(cpu.y & 0xff00, 0x0000, "y");
    assert!(cpu.signals.e && cpu.flags.emulation, "emulation");
    assert_eq!(cpu.signals.mx(false), true, "mx:m");
    assert_eq!(cpu.signals.mx(true), true, "mx:x");
    assert_eq!(cpu.flags.mem_sel, true, "m");
    assert_eq!(cpu.flags.index_sel, true, "x");
    assert_eq!(cpu.flags.decimal, false, "d");
    assert_eq!(cpu.flags.interrupt_disable, true, "i");
    assert_eq!(cpu.flags.carry, true, "c");
}
