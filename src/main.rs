use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Reg {
    R0,
    R1,
    R2,
    R3,
}

impl From<Reg> for u16 {
    fn from(val: Reg) -> Self {
        match val {
            Reg::R0 => 0,
            Reg::R1 => 1,
            Reg::R2 => 2,
            Reg::R3 => 3,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Syscall {
    Write,
    Exit,
}

#[derive(Debug, Clone)]
enum Instr {
    Label(String),       // a label to jump to
    Mov(Reg, Reg),       // r0 = r1
    Imm(Reg, u16),       // r0 = imm
    Load(Reg, u16),      // r0 = stack[imm..imm+2]
    Store(u16, Reg),     // stack[imm..imm+2] = r0
    Add(Reg, Reg, Reg),  // r0 = r1 + r2
    Sub(Reg, Reg, Reg),  // r0 = r1 - r2
    Mul(Reg, Reg, Reg),  // r0 = r1 * r2
    Div(Reg, Reg, Reg),  // r0 = r1 / r2
    Jump(String),        // pc = label
    JumpIf(Reg, String), // if r3 > 0, jump.
    Call(String),        // pc = label
    Ret,                 // pc = back
    Syscall(Syscall),    // 0 = open, 1 = create, 2 = read, 3 = write
    LoadInd(Reg, Reg),   // r0 = mem[r1]
    StoreInd(Reg, Reg),  // mem[r0] = r1
}

struct VM {
    regs: [u16; 4],
    sp: u16,
    pc: u16,
    mem: [u8; 65536],
    program: Vec<Instr>,
    ret: Vec<u16>, // stack of addresses to return to
    labels: HashMap<String, u16>,
}

const CALLER_SAVED_REGS: [Reg; 3] = [Reg::R1, Reg::R2, Reg::R3];

impl VM {
    fn new() -> Self {
        Self {
            regs: Default::default(),
            sp: Default::default(),
            pc: Default::default(),
            mem: [0; 65536],
            ret: Default::default(),
            program: Default::default(),
            labels: Default::default(),
        }
    }

    fn read(&self, addr: u16) -> u16 {
        let a = addr as usize;
        u16::from_le_bytes(self.mem[a..a + 2].try_into().unwrap())
    }

    fn write(&mut self, addr: u16, val: u16) {
        let a = addr as usize;
        self.mem[a..a + 2].copy_from_slice(&val.to_le_bytes());
    }

    fn read_reg(&self, reg: &Reg) -> u16 {
        match reg {
            Reg::R0 => self.regs[0],
            Reg::R1 => self.regs[1],
            Reg::R2 => self.regs[2],
            Reg::R3 => self.regs[3],
        }
    }

    fn write_reg(&mut self, reg: &Reg, val: u16) {
        match reg {
            Reg::R0 => self.regs[0] = val,
            Reg::R1 => self.regs[1] = val,
            Reg::R2 => self.regs[2] = val,
            Reg::R3 => self.regs[3] = val,
        }
    }

    fn run(&mut self, prog: &[Instr]) {
        self.program = prog.to_vec();

        for (i, instr) in self.program.iter().enumerate() {
            if let Instr::Label(label) = instr {
                self.labels.insert(label.to_string(), i as u16);
            }
        }

        let program = self.program.clone();

        while self.pc < program.len() as u16 {
            match &program[self.pc as usize] {
                Instr::Label(_) => self.pc += 1, // first look through labels before running, so skip
                Instr::Mov(r0, r1) => {
                    self.write_reg(r0, self.read_reg(r1));
                    self.pc += 1
                }
                Instr::Imm(reg, val) => {
                    self.write_reg(reg, *val);
                    self.pc += 1
                }
                Instr::Load(reg, val) => {
                    let v = self.read(*val);
                    self.write_reg(reg, v);
                    self.pc += 1
                }
                Instr::Store(val, reg) => {
                    let v = self.read_reg(reg);
                    self.write(*val, v);
                    self.pc += 1
                }
                Instr::Add(r0, r1, r2) => {
                    self.write_reg(r0, self.read_reg(r1) + self.read_reg(r2));
                    self.pc += 1
                }
                Instr::Sub(r0, r1, r2) => {
                    self.write_reg(r0, self.read_reg(r1) - self.read_reg(r2));
                    self.pc += 1
                }
                Instr::Mul(r0, r1, r2) => {
                    self.write_reg(r0, self.read_reg(r1) * self.read_reg(r2));
                    self.pc += 1
                }
                Instr::Div(r0, r1, r2) => {
                    self.write_reg(r0, self.read_reg(r1) / self.read_reg(r2));
                    self.pc += 1
                }
                Instr::Jump(label) => {
                    self.pc = *self.labels.get(label).unwrap();
                }
                Instr::JumpIf(reg, label) => {
                    if self.read_reg(reg) > 0 {
                        self.pc = *self.labels.get(label).unwrap();
                    } else {
                        self.pc += 1;
                    }
                }
                Instr::Call(label) => {
                    for (i, reg) in CALLER_SAVED_REGS.iter().enumerate() {
                        let val = self.read_reg(reg);
                        let addr = self.sp.wrapping_add((i as u16) * 2);
                        self.write(addr, val);
                    }
                    self.sp = self.sp.wrapping_add((CALLER_SAVED_REGS.len() * 2) as u16);
                    self.ret.push(self.pc + 1);
                    self.pc = *self.labels.get(label).unwrap();
                }
                Instr::Ret => {
                    let return_addr = match self.ret.pop() {
                        Some(addr) => addr,
                        None => {
                            println!("Return code: {}", self.regs[0]);
                            break;
                        }
                    };

                    self.sp = self.sp.wrapping_sub((CALLER_SAVED_REGS.len() * 2) as u16);
                    for (i, reg) in CALLER_SAVED_REGS.iter().enumerate() {
                        let addr = self.sp.wrapping_add((i as u16) * 2);
                        let val = self.read(addr);
                        self.write_reg(reg, val);
                    }

                    self.pc = return_addr;
                }
                Instr::Syscall(syscall) => match syscall {
                    Syscall::Write => {
                        println!("{}", self.regs[0]);
                        self.pc += 1
                    }
                    Syscall::Exit => break,
                },
                Instr::LoadInd(dest, addr) => {
                    let a = self.read_reg(addr);
                    let val = self.read(a);
                    self.write_reg(dest, val);
                    self.pc += 1;
                }
                Instr::StoreInd(addr, src) => {
                    let a = self.read_reg(addr);
                    let val = self.read_reg(src);
                    self.write(a, val);
                    self.pc += 1;
                }
            }
        }
    }
}

// Working factorial example:
fn main() {
    let mut vm = VM::new();

    let prog = vec![
        Instr::Label("main".into()),    // main:
        Instr::Imm(Reg::R0, 6),         //   r0 = 6
        Instr::Call("fact".into()),     //   call fact
        Instr::Syscall(Syscall::Write), //   print r0
        Instr::Syscall(Syscall::Exit),  //   exit
        Instr::Label("fact".into()),    // fact:
        Instr::Mov(Reg::R2, Reg::R0),   //   r2 = n
        Instr::Imm(Reg::R1, 1),         //   r1 = 1
        // r0 = n - 1  (was previously 1 - n)
        Instr::Sub(Reg::R0, Reg::R2, Reg::R1),
        // if r0 > 0 jump to recurse; else fall through to base case
        Instr::JumpIf(Reg::R0, "recurse".into()),
        // base case: r0 = 1; return
        Instr::Imm(Reg::R0, 1),
        Instr::Ret,
        Instr::Label("recurse".into()), // recurse:
        Instr::Call("fact".into()),     //   r0 = fact(n-1)
        // r0 = n * fact(n-1)  (was previously storing into r2)
        Instr::Mul(Reg::R0, Reg::R2, Reg::R0),
        Instr::Ret,
    ];

    let memcpy = vec![
        // ─── main ───────────────────────────────────────
        Instr::Label("main".into()),
        // 1) write {1,2,3} into source at 0x0100,0x0102,0x0104
        Instr::Imm(Reg::R0, 1),
        Instr::Store(0x0100, Reg::R0),
        Instr::Imm(Reg::R0, 2),
        Instr::Store(0x0102, Reg::R0),
        Instr::Imm(Reg::R0, 3),
        Instr::Store(0x0104, Reg::R0),
        // 2) set up memcpy arguments:
        //    R0 = src_ptr, R1 = dst_ptr, R2 = len_in_words
        Instr::Imm(Reg::R0, 0x0100), // src = 0x0100
        Instr::Imm(Reg::R1, 0x0200), // dst = 0x0200
        Instr::Imm(Reg::R2, 3),      // len = 3 words
        Instr::Call("memcpy".into()),
        // 3) reset for printing
        Instr::Imm(Reg::R1, 0x0200), // dst = 0x0200
        Instr::Imm(Reg::R2, 3),      // count = 3
        // 4) print_loop: load from [R1], print, ptr+=2, count--
        Instr::Label("print_loop".into()),
        Instr::LoadInd(Reg::R0, Reg::R1),
        Instr::Syscall(Syscall::Write),
        Instr::Imm(Reg::R0, 2),
        Instr::Add(Reg::R1, Reg::R1, Reg::R0), // dst_ptr += 2
        Instr::Imm(Reg::R0, 1),
        Instr::Sub(Reg::R2, Reg::R2, Reg::R0), // count--
        Instr::JumpIf(Reg::R2, "print_loop".into()),
        Instr::Syscall(Syscall::Exit),
        // ─── memcpy subroutine ───────────────────────────
        Instr::Label("memcpy".into()),
        // if len (R2) == 0, return immediately
        Instr::JumpIf(Reg::R2, "copy_loop".into()),
        Instr::Ret,
        Instr::Label("copy_loop".into()),
        // load one word from [R0] → R3
        Instr::LoadInd(Reg::R3, Reg::R0),
        // store that word from R3 → [R1]
        Instr::StoreInd(Reg::R1, Reg::R3),
        // decrement length: R2 = R2 - 1
        Instr::Imm(Reg::R3, 1),
        Instr::Sub(Reg::R2, Reg::R2, Reg::R3),
        // bump src ptr: R0 = R0 + 2
        Instr::Imm(Reg::R3, 2),
        Instr::Add(Reg::R0, Reg::R0, Reg::R3),
        // bump dst ptr: R1 = R1 + 2
        Instr::Imm(Reg::R3, 2),
        Instr::Add(Reg::R1, Reg::R1, Reg::R3),
        // loop back if R2 > 0
        Instr::JumpIf(Reg::R2, "copy_loop".into()),
        Instr::Ret,
    ];

    vm.run(&memcpy);
}
