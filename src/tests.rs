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
            cpu.cycle(&mut sys);
            black_box(());
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

fn run_cmp_imm_8(a: u8, m: u8) -> (bool, bool, bool) {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    // LDA #a ; CMP #m (8-bit emulation mode)
    sys.write_code(0x008000, &[0xA9, a, 0xC9, m]);
    for _ in 0..7 + 2 + 2 {
        cpu.cycle(&mut sys);
    }
    (cpu.flags.negative, cpu.flags.zero, cpu.flags.carry)
}

fn run_cmp_imm_16(a: u16, m: u16) -> (bool, bool, bool) {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    // CLC; XCE; CLC; REP #$30; LDA #a; CMP #m  (16-bit mode)
    sys.write_code(
        0x008000,
        &[
            0x18,                                 // CLC
            0xFB,                                 // XCE
            0x18,                                 // CLC
            0xC2, 0x30,                           // REP #$30
            0xA9, (a & 0xff) as u8, (a >> 8) as u8,   // LDA #a
            0xC9, (m & 0xff) as u8, (m >> 8) as u8,   // CMP #m
        ],
    );
    // 7 reset + 2+2+2+3+3+3 instructions
    for _ in 0..7 + 2 + 2 + 2 + 3 + 3 + 3 {
        cpu.cycle(&mut sys);
    }
    (cpu.flags.negative, cpu.flags.zero, cpu.flags.carry)
}

#[test]
fn cmp_immediate_8bit() {
    // A < M: N=1, Z=0, C=0
    let (n, z, c) = run_cmp_imm_8(0x40, 0x60);
    assert!(n, "n: A<M");
    assert!(!z, "z: A<M");
    assert!(!c, "c: A<M");

    // A == M: N=0, Z=1, C=1
    let (n, z, c) = run_cmp_imm_8(0x50, 0x50);
    assert!(!n, "n: A==M");
    assert!(z, "z: A==M");
    assert!(c, "c: A==M");

    // A > M: N=0, Z=0, C=1
    let (n, z, c) = run_cmp_imm_8(0x70, 0x50);
    assert!(!n, "n: A>M");
    assert!(!z, "z: A>M");
    assert!(c, "c: A>M");
}

#[test]
fn cmp_immediate_16bit() {
    // A < M: N=1, Z=0, C=0
    let (n, z, c) = run_cmp_imm_16(0x1234, 0x2000);
    assert!(n, "n: A<M");
    assert!(!z, "z: A<M");
    assert!(!c, "c: A<M");

    // A == M: N=0, Z=1, C=1
    let (n, z, c) = run_cmp_imm_16(0x1234, 0x1234);
    assert!(!n, "n: A==M");
    assert!(z, "z: A==M");
    assert!(c, "c: A==M");

    // A > M: N=0, Z=0, C=1
    let (n, z, c) = run_cmp_imm_16(0x8000, 0x1000);
    assert!(!n, "n: A>M");
    assert!(!z, "z: A>M");
    assert!(c, "c: A>M");
}

// Returns (result_a_low, carry, zero, negative) after ROL accumulator (8-bit)
fn run_rol_acc_8(a: u8, carry_in: bool) -> (u8, bool, bool, bool) {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    let mut code = vec![0xA9u8, a]; // LDA #a
    if carry_in {
        code.push(0x38); // SEC
    } else {
        code.push(0x18); // CLC
    }
    code.push(0x2A); // ROL A
    sys.write_code(0x008000, &code);
    // 7 reset + 2 (LDA) + 2 (SEC/CLC) + 2 (ROL A)
    for _ in 0..7 + 2 + 2 + 2 {
        cpu.cycle(&mut sys);
    }
    (
        (cpu.a & 0xff) as u8,
        cpu.flags.carry,
        cpu.flags.zero,
        cpu.flags.negative,
    )
}

// Returns (result_a_low, carry, zero, negative) after ROR accumulator (8-bit)
fn run_ror_acc_8(a: u8, carry_in: bool) -> (u8, bool, bool, bool) {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    let mut code = vec![0xA9u8, a]; // LDA #a
    if carry_in {
        code.push(0x38); // SEC
    } else {
        code.push(0x18); // CLC
    }
    code.push(0x6A); // ROR A
    sys.write_code(0x008000, &code);
    for _ in 0..7 + 2 + 2 + 2 {
        cpu.cycle(&mut sys);
    }
    (
        (cpu.a & 0xff) as u8,
        cpu.flags.carry,
        cpu.flags.zero,
        cpu.flags.negative,
    )
}

#[test]
fn rol_accumulator_8bit() {
    // Basic rotate: 0x02 << 1 = 0x04, carry_in=0 -> carry_out=0
    let (a, c, z, n) = run_rol_acc_8(0x02, false);
    assert_eq!(a, 0x04, "a");
    assert!(!c, "c");
    assert!(!z, "z");
    assert!(!n, "n");

    // Carry-in feeds bit 0: 0x02, carry_in=1 -> 0x05
    let (a, c, z, n) = run_rol_acc_8(0x02, true);
    assert_eq!(a, 0x05, "a carry-in");
    assert!(!c, "c carry-in");
    assert!(!z, "z carry-in");
    assert!(!n, "n carry-in");

    // Carry-out from bit 7: 0x80, carry_in=0 -> result=0, carry=1, zero=1
    let (a, c, z, n) = run_rol_acc_8(0x80, false);
    assert_eq!(a, 0x00, "a carry-out");
    assert!(c, "c carry-out");
    assert!(z, "z carry-out");
    assert!(!n, "n carry-out");

    // Negative flag: 0x40, carry_in=0 -> 0x80, N=1
    let (a, c, z, n) = run_rol_acc_8(0x40, false);
    assert_eq!(a, 0x80, "a negative");
    assert!(!c, "c negative");
    assert!(!z, "z negative");
    assert!(n, "n negative");
}

#[test]
fn ror_accumulator_8bit() {
    // Basic rotate: 0x08 >> 1 = 0x04, carry_in=0 -> carry_out=0
    let (a, c, z, n) = run_ror_acc_8(0x08, false);
    assert_eq!(a, 0x04, "a");
    assert!(!c, "c");
    assert!(!z, "z");
    assert!(!n, "n");

    // Carry-in feeds bit 7: 0x02, carry_in=1 -> 0x81
    let (a, c, z, n) = run_ror_acc_8(0x02, true);
    assert_eq!(a, 0x81, "a carry-in");
    assert!(!c, "c carry-in");
    assert!(!z, "z carry-in");
    assert!(n, "n carry-in");

    // Carry-out from bit 0: 0x01, carry_in=0 -> result=0, carry=1, zero=1
    let (a, c, z, n) = run_ror_acc_8(0x01, false);
    assert_eq!(a, 0x00, "a carry-out");
    assert!(c, "c carry-out");
    assert!(z, "z carry-out");
    assert!(!n, "n carry-out");

    // Negative via carry-in: 0x00, carry_in=1 -> 0x80
    let (a, c, z, n) = run_ror_acc_8(0x00, true);
    assert_eq!(a, 0x80, "a negative");
    assert!(!c, "c negative");
    assert!(!z, "z negative");
    assert!(n, "n negative");
}

#[test]
fn rol_accumulator_16bit() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    // CLC; XCE; REP #$20; LDA #$4000; SEC; ROL A
    sys.write_code(
        0x008000,
        &[
            0x18,       // CLC
            0xFB,       // XCE  (native mode)
            0xC2, 0x20, // REP #$20  (16-bit accumulator)
            0xA9, 0x00, 0x40, // LDA #$4000
            0x38,       // SEC
            0x2A,       // ROL A
        ],
    );
    // 7 reset + 2+2+3+3+2+2
    for _ in 0..7 + 2 + 2 + 3 + 3 + 2 + 2 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.a, 0x8001, "a 16-bit rol");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
    assert!(cpu.flags.negative, "n");
}

#[test]
fn ror_accumulator_16bit() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    // CLC; XCE; REP #$20; LDA #$0002; CLC; ROR A
    sys.write_code(
        0x008000,
        &[
            0x18,       // CLC
            0xFB,       // XCE
            0xC2, 0x20, // REP #$20
            0xA9, 0x02, 0x00, // LDA #$0002
            0x18,       // CLC
            0x6A,       // ROR A
        ],
    );
    for _ in 0..7 + 2 + 2 + 3 + 3 + 2 + 2 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.a, 0x0001, "a 16-bit ror");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
    assert!(!cpu.flags.negative, "n");
}

#[test]
fn rol_direct_8bit() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x0010] = 0x42;
    // CLC; ROL $10
    sys.write_code(0x008000, &[0x18, 0x26, 0x10]);
    // 7 reset + 2 (CLC) + 5 (ROL dp)
    for _ in 0..7 + 2 + 5 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(sys.ram[0x0010], 0x84, "mem after rol direct");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
    assert!(cpu.flags.negative, "n");
}

#[test]
fn ror_direct_8bit() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x0010] = 0x42;
    // SEC; ROR $10
    sys.write_code(0x008000, &[0x38, 0x66, 0x10]);
    // 7 reset + 2 (SEC) + 5 (ROR dp)
    for _ in 0..7 + 2 + 5 {
        cpu.cycle(&mut sys);
    }
    // 0x42 = 0b01000010, carry_in=1 -> 0b10100001 = 0xA1, carry_out=0
    assert_eq!(sys.ram[0x0010], 0xA1, "mem after ror direct");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
    assert!(cpu.flags.negative, "n");
}

#[test]
fn lsr_accumulator_8bit() {
    // 0x80 >> 1 = 0x40, carry=0, N always cleared
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.write_code(0x008000, &[0xA9, 0x80, 0x4A]); // LDA #$80; LSR A
    for _ in 0..7 + 2 + 2 {
        cpu.cycle(&mut sys);
    }
    assert_eq!((cpu.a & 0xff) as u8, 0x40, "a");
    assert!(!cpu.flags.carry, "c");
    assert!(!cpu.flags.zero, "z");
    assert!(!cpu.flags.negative, "n");

    // 0x01 >> 1 = 0x00, carry=1, zero=1
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.write_code(0x008000, &[0xA9, 0x01, 0x4A]); // LDA #$01; LSR A
    for _ in 0..7 + 2 + 2 {
        cpu.cycle(&mut sys);
    }
    assert_eq!((cpu.a & 0xff) as u8, 0x00, "a carry-out");
    assert!(cpu.flags.carry, "c carry-out");
    assert!(cpu.flags.zero, "z carry-out");
    assert!(!cpu.flags.negative, "n always clear");
}

#[test]
fn tsb_direct_8bit() {
    // M=0x0F, A=0xF0 -> M|=A => 0xFF; Z=1 (no overlap: A & M == 0)
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x0010] = 0x0F;
    sys.write_code(0x008000, &[0xA9, 0xF0, 0x04, 0x10]); // LDA #$F0; TSB $10
    // 7 reset + 2 (LDA imm) + 5 (TSB dp)
    for _ in 0..7 + 2 + 5 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(sys.ram[0x0010], 0xFF, "mem after tsb");
    assert!(cpu.flags.zero, "z (no overlap)");

    // M=0xFF, A=0x0F -> M|=A => 0xFF; Z=0 (overlap exists)
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x0010] = 0xFF;
    sys.write_code(0x008000, &[0xA9, 0x0F, 0x04, 0x10]); // LDA #$0F; TSB $10
    for _ in 0..7 + 2 + 5 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(sys.ram[0x0010], 0xFF, "mem after tsb overlap");
    assert!(!cpu.flags.zero, "z (overlap)");
}

#[test]
fn xba_test() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    // LDA #$BB (high byte via XCE/REP later) — use XBA directly by setting cpu.a
    // Place: LDA #$AA (imm 8-bit in emulation mode), then set B manually, then XBA
    // Simpler: just set cpu.a after reset and run XBA opcode
    sys.write_code(0x008000, &[0xEB]); // XBA
    for _ in 0..7 {
        cpu.cycle(&mut sys); // reset sequence
    }
    cpu.a = 0xBBAA;
    // 3 cycles: fetch(0xEB) + 2 IO
    for _ in 0..3 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.a, 0xAABB, "a after xba");
    assert!(cpu.flags.negative, "n flag (0xAA bit7=1)");
    assert!(!cpu.flags.zero, "z flag");
}

#[test]
fn xba_zero_flag() {
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.write_code(0x008000, &[0xEB]); // XBA
    for _ in 0..7 {
        cpu.cycle(&mut sys);
    }
    cpu.a = 0x00AA; // B=0x00, A=0xAA -> after XBA: A=0x00, B=0xAA, N=0, Z=1
    for _ in 0..3 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.a, 0xAA00, "a after xba zero");
    assert!(cpu.flags.zero, "z flag set when new A=0");
    assert!(!cpu.flags.negative, "n flag clear");
}

#[test]
fn brl_zero_offset() {
    // BRL $0000 at 0x8000: PC should become 0x8003 (3-byte instruction + 0 offset)
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.write_code(0x008000, &[0x82, 0x00, 0x00]); // BRL $0000
    // 7 reset + 1 fetch + 3 brl cycles
    for _ in 0..7 + 1 + 3 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.pc, 0x8003, "pc after brl +0");
    assert_eq!(cpu.pbr, 0x00, "pbr unchanged");
}

#[test]
fn brl_forward_offset() {
    // BRL $0010 at 0x8000: PC = 0x8003 + 0x0010 = 0x8013
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.write_code(0x008000, &[0x82, 0x10, 0x00]); // BRL +16
    for _ in 0..7 + 1 + 3 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.pc, 0x8013, "pc after brl +16");
}

#[test]
fn brl_backward_offset() {
    // BRL $FFFD (-3) at 0x8000: PC = 0x8003 + (-3) = 0x8000
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.write_code(0x008000, &[0x82, 0xFD, 0xFF]); // BRL -3 (loops back to itself)
    for _ in 0..7 + 1 + 3 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.pc, 0x8000, "pc after brl -3");
}

#[test]
fn jml_indirect_long() {
    // JML [$2000]: pointer at 0x2000 = [0x34, 0x12, 0x05] -> PC=0x1234, PBR=0x05
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x002000] = 0x34;
    sys.ram[0x002001] = 0x12;
    sys.ram[0x002002] = 0x05;
    sys.write_code(0x008000, &[0xDC, 0x00, 0x20]); // JML [$2000]
    // 7 reset + 1 fetch + 5 jml cycles
    for _ in 0..7 + 1 + 5 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(cpu.pc, 0x1234, "pc after jml");
    assert_eq!(cpu.pbr, 0x05, "pbr after jml");
}

#[test]
fn per_zero_offset() {
    // PER $0000 at 0x8000: pushes 0x8003 (PC+3+0)
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.write_code(0x008000, &[0x62, 0x00, 0x00]); // PER $0000
    // 7 reset + 1 fetch + 5 per cycles
    for _ in 0..7 + 1 + 5 {
        cpu.cycle(&mut sys);
    }
    let s = cpu.s;
    let lo = sys.ram[s as usize + 1] as u16;
    let hi = sys.ram[s as usize + 2] as u16;
    let pushed = (hi << 8) | lo;
    assert_eq!(pushed, 0x8003, "per pushed value");
}

#[test]
fn trb_direct_8bit() {
    // M=0xFF, A=0x0F -> M&=~A => 0xF0; Z=0 (overlap exists)
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x0010] = 0xFF;
    sys.write_code(0x008000, &[0xA9, 0x0F, 0x14, 0x10]); // LDA #$0F; TRB $10
    // 7 reset + 2 (LDA imm) + 5 (TRB dp)
    for _ in 0..7 + 2 + 5 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(sys.ram[0x0010], 0xF0, "mem after trb");
    assert!(!cpu.flags.zero, "z (overlap)");

    // M=0x0F, A=0xF0 -> M&=~A => 0x0F; Z=1 (no overlap)
    let mut cpu = CPU::new();
    let mut sys = Sys::default();
    sys.ram[0xfffc] = 0x00;
    sys.ram[0xfffd] = 0x80;
    sys.ram[0x0010] = 0x0F;
    sys.write_code(0x008000, &[0xA9, 0xF0, 0x14, 0x10]); // LDA #$F0; TRB $10
    for _ in 0..7 + 2 + 5 {
        cpu.cycle(&mut sys);
    }
    assert_eq!(sys.ram[0x0010], 0x0F, "mem after trb no-overlap");
    assert!(cpu.flags.zero, "z (no overlap)");
}
