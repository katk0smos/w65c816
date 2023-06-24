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
    fn read(&mut self, a: u32, _at: AddressType, _signals: &Signals) -> u8 {
        self.ram[(a & 0x00ffff) as usize]
    }

    fn write(&mut self, a: u32, d: u8, _signals: &Signals) {
        self.ram[(a & 0xffffff) as usize] = d;
    }

    fn res(&mut self) -> bool {
        let x = self.res;

        if !x {
            self.res = true;
        }

        !x
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
    assert!(cpu.signals.e, "emulation");
    assert!(cpu.signals.mx(false), "mx:m");
    assert!(cpu.signals.mx(true), "mx:x");
    assert!(cpu.flags.mem_sel, "m");
    assert!(cpu.flags.index_sel, "x");
    assert!(!cpu.flags.decimal, "d");
    assert!(cpu.flags.interrupt_disable, "i");
    assert!(cpu.flags.carry, "c");
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
    assert_eq!(cpu.x, 0x0023, "x");
    assert_eq!(cpu.y, 0x0095, "y");
    assert!(cpu.signals.e && cpu.flags.emulation, "emulation");
    assert!(cpu.signals.e, "emulation");
    assert!(cpu.signals.mx(false), "mx:m");
    assert!(cpu.signals.mx(true), "mx:x");
    assert!(cpu.flags.mem_sel, "m");
    assert!(cpu.flags.index_sel, "x");
    assert!(!cpu.flags.decimal, "d");
    assert!(cpu.flags.interrupt_disable, "i");
    assert!(cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
    assert!(!cpu.flags.negative, "n");
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
    assert!(cpu.signals.mx(false), "mx:m");
    assert!(cpu.signals.mx(true), "mx:x");
    assert!(cpu.flags.mem_sel, "m");
    assert!(cpu.flags.index_sel, "x");
    assert!(!cpu.flags.decimal, "d");
    assert!(cpu.flags.interrupt_disable, "i");
    assert!(cpu.flags.carry, "c");
}

#[test]
fn lda() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;

    sys.ram[0x4000] = 12;
    sys.ram[0x4001] = 34;

    sys.ram[0x8000..0x8000 + 14].copy_from_slice(&[
        0xA9, 0x00, // LDA #$00
        0xA0, 0x00, // LDX #$00
        0xA2, 0x00, // LDY #$00
        0x18, // CLC
        0xFB, // XCE
        0x18, // CLC
        0xC2, 0x30, // REP MX
        0xAD, 0x00, 0x40, // LDA $4000
    ]);

    for i in 0..8 + 20 {
        cpu.cycle(&mut sys);
        println!("{} {:?}", i, cpu.state);
    }

    println!("{:#?}", cpu);

    assert_eq!(cpu.pc, 0x800e);
    assert_eq!(cpu.dbr, 00, "dbr");
    assert_eq!(cpu.pbr, 00, "pbr");
    assert_eq!(cpu.d, 0x0000, "d");
    assert_eq!(cpu.s & 0xff00, 0x0100, "s");
    assert_eq!(cpu.a, 0x220c, "a");
    assert_eq!(cpu.x & 0xff00, 0x0000, "x");
    assert_eq!(cpu.y & 0xff00, 0x0000, "y");
    assert!(!(cpu.signals.e && cpu.flags.emulation), "emulation");
    assert!(!cpu.signals.mx(false), "mx:m");
    assert!(!cpu.signals.mx(true), "mx:x");
    assert!(!cpu.flags.mem_sel, "m");
    assert!(!cpu.flags.index_sel, "x");
    assert!(!cpu.flags.decimal, "d");
    assert!(cpu.flags.interrupt_disable, "i");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
}

#[test]
fn st_zp() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;

    sys.ram[0x4000] = 12;
    sys.ram[0x4001] = 34;

    const CODE: &[u8] = &[
        0xA9, 0x00, // LDA #$00
        0x18, // CLC
        0xFB, // XCE
        0x18, // CLC
        0xC2, 0x30, // REP MX
        0xAD, 0x00, 0x40, // LDA $4000
        0xE2, 0x30, // SEP MX
        0x85, 0, // STA $00
    ];

    sys.ram[0x8000..0x8000 + CODE.len()].copy_from_slice(CODE);

    for _ in 0..8 + 9 + 5 + 2 + 4 + 2 {
        cpu.cycle(&mut sys);
        println!("{:?}", cpu.state);
    }

    assert_eq!(cpu.pc, 0x8000 + CODE.len() as u16);
    assert_eq!(sys.ram[0], 12, "sta");
    assert_eq!(sys.ram[1], 0xEA, "sta");
    assert_eq!(cpu.dbr, 00, "dbr");
    assert_eq!(cpu.pbr, 00, "pbr");
    assert_eq!(cpu.d, 0x0000, "d");
    assert_eq!(cpu.s & 0xff00, 0x0100, "s");
    assert_eq!(cpu.a, 0x220c, "a");
    assert_eq!(cpu.x & 0xff00, 0x0000, "x");
    assert_eq!(cpu.y & 0xff00, 0x0000, "y");
    assert!(!(cpu.signals.e && cpu.flags.emulation), "emulation");
    assert!(cpu.signals.mx(false), "mx:m");
    assert!(cpu.signals.mx(true), "mx:x");
    assert!(cpu.flags.mem_sel, "m");
    assert!(cpu.flags.index_sel, "x");
    assert!(!cpu.flags.decimal, "d");
    assert!(cpu.flags.interrupt_disable, "i");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
}
