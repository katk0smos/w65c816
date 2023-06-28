use super::*;

struct Sys {
    ram: Box<[u8]>,
    res: bool,
    irq: bool,
    nmi: bool,
    abort: bool,
}

impl Default for Sys {
    fn default() -> Self {
        Self {
            ram: vec![0xEAu8; 0x1000000].into_boxed_slice(),
            res: true,
            irq: false,
            nmi: false,
            abort: false,
        }
    }
}

impl Sys {
    fn res(&mut self) {
        self.res = true;
    }

    fn nmi(&mut self) {
        self.nmi = true;
    }

    fn irq(&mut self) {
        self.irq = true;
    }

    fn write_code(&mut self, addr: u32, code: &[u8]) {
        self.ram[addr as usize..addr as usize + code.len()].copy_from_slice(code);
    }
}

impl System for Sys {
    fn read(&mut self, a: u32, _at: AddressType, _signals: &Signals) -> u8 {
        let d = self.ram[(a & 0xffffff) as usize];
        println!("{:06x} -> {:02x}", a, d);
        d
    }

    fn write(&mut self, a: u32, d: u8, at: AddressType, _signals: &Signals) {
        println!("{:06x} <- {:02x}", a, d);
        if at != AddressType::Invalid {
            self.ram[(a & 0xffffff) as usize] = d;
        }
    }

    fn res(&mut self) -> bool {
        let x = self.res;

        if x {
            self.res = false;
        }

        x
    }

    fn nmi(&mut self) -> bool {
        let x = self.nmi;

        if x {
            self.nmi = false;
        }

        x
    }

    fn irq(&mut self) -> bool {
        let x = self.irq;

        if x {
            self.irq = false;
        }

        x
    }
}

#[test]
fn reset() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;

    for _ in 0..7 {
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

#[bench]
fn nops_bench(b: &mut test::Bencher) {
    use test::black_box;

    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x02;

    for _ in 0..7 {
        cpu.cycle(&mut sys);
    }

    b.iter(|| {
        let mut cpu = cpu.clone();

        for _ in 0..2 {
            black_box(cpu.cycle(&mut sys));
        }
    });
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

    for _ in 0..7 + 2 * 3 {
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

    for _ in 0..7 + (0xFFFF - 0xEAEA + 0x0100) * 2 {
        cpu.cycle(&mut sys);
        println!("{:02x}{:04x}: {:?}", cpu.pbr, cpu.pc, cpu.state);
    }

    assert_eq!(cpu.pc, 0x00ff);
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

    sys.write_code(
        0x008000,
        &[
            0xA9, 0x00, // LDA #$00
            0xA0, 0x00, // LDX #$00
            0xA2, 0x00, // LDY #$00
            0x18, // CLC
            0xFB, // XCE
            0x18, // CLC
            0xC2, 0x30, // REP MX
            0xAD, 0x00, 0x40, // LDA $4000
        ],
    );

    for i in 0..7 + 20 {
        cpu.cycle(&mut sys);
    }

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

    sys.write_code(0x008000, CODE);

    for _ in 0..7 + 9 + 5 + 2 + 4 + 2 {
        cpu.cycle(&mut sys);
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

#[test]
fn wai_irq_i_special_behavior() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;

    const CODE: &[u8] = &[
        0x18, // CLC
        0xA9, 0x00, // LDA #$00
        0xCB, // WAI
        0xA9, 0xff, // LDA #$ff
    ];

    sys.write_code(0x008000, CODE);

    for _ in 0..7 + 2 + 5 {
        cpu.cycle(&mut sys);
    }

    assert!(cpu.wai, "wai");
    assert_eq!(cpu.a & 0xff, 0x00, "a");

    for _ in 0..10 {
        cpu.cycle(&mut sys);
    }

    sys.irq();

    for _ in 0..2 {
        cpu.cycle(&mut sys);
    }

    assert_eq!(cpu.pc, 0x8000 + CODE.len() as u16);
    assert_eq!(cpu.dbr, 00, "dbr");
    assert_eq!(cpu.pbr, 00, "pbr");
    assert_eq!(cpu.d, 0x0000, "d");
    assert_eq!(cpu.s & 0xff00, 0x0100, "s");
    assert_eq!(cpu.a & 0xff, 0xff, "a");
    assert_eq!(cpu.x & 0xff00, 0x0000, "x");
    assert_eq!(cpu.y & 0xff00, 0x0000, "y");
    assert!((cpu.signals.e && cpu.flags.emulation), "emulation");
    assert!(cpu.signals.mx(false), "mx:m");
    assert!(cpu.signals.mx(true), "mx:x");
    assert!(cpu.flags.mem_sel, "m");
    assert!(cpu.flags.index_sel, "x");
    assert!(!cpu.flags.decimal, "d");
    assert!(cpu.flags.interrupt_disable, "i");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
}

#[test]
fn irq() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0xfffe] = 0x00;
    sys.ram[0xffff] = 0x90;

    const CODE: &[u8] = &[
        0x18, // CLC
        0xA9, 0x00, // LDA #$00
        0x58, // CLI
        0xCB, // WAI
        0xA9, 0xff, // LDA #$ff
    ];

    const CODE2: &[u8] = &[
        0xA9, 0x0F, // LDA #$7f
        0x40, // RTI
    ];

    sys.write_code(0x008000, CODE);
    sys.write_code(0x009000, CODE2);

    for _ in 0..7 + 2 + 2 + 5 {
        println!("{:?}", cpu.state);
        cpu.cycle(&mut sys);
    }

    println!("{:?}", cpu.state);

    assert!(cpu.wai, "wai");
    assert_eq!(cpu.a & 0xff, 0x00, "a");

    for _ in 0..10 {
        cpu.cycle(&mut sys);
    }

    sys.irq();

    for _ in 0..7 {
        cpu.cycle(&mut sys);
        println!("{:?}", cpu.state);
    }

    assert_eq!(cpu.pc, 0x9000, "pc");
    assert!(cpu.flags.interrupt_disable, "i");

    for _ in 0..8 {
        cpu.cycle(&mut sys);
        println!("{:?} {:02x}", cpu.state, sys.ram[cpu.s as usize]);
    }

    assert_eq!(cpu.pc, 0x8000 + CODE.len() as u16 - 2);
    assert_eq!(cpu.dbr, 00, "dbr");
    assert_eq!(cpu.pbr, 00, "pbr");
    assert_eq!(cpu.d, 0x0000, "d");
    assert_eq!(cpu.s & 0xff00, 0x0100, "s");
    assert_eq!(cpu.a & 0xff, 0x0f, "a");
    assert_eq!(cpu.x & 0xff00, 0x0000, "x");
    assert_eq!(cpu.y & 0xff00, 0x0000, "y");
    assert!((cpu.signals.e && cpu.flags.emulation), "emulation");
    assert!(cpu.signals.mx(false), "mx:m");
    assert!(cpu.signals.mx(true), "mx:x");
    assert!(cpu.flags.mem_sel, "m");
    assert!(cpu.flags.index_sel, "x");
    assert!(!cpu.flags.decimal, "d");
    assert!(!cpu.flags.interrupt_disable, "i");
    assert!(!cpu.flags.carry, "c");
    assert!(cpu.flags.zero, "z");
}
