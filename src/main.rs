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
    Sub(Reg, Reg, Reg),  // r0 = r1 - r2
    Mul(Reg, Reg, Reg),  // r0 = r1 * r2
    Jump(String),        // pc = label
    JumpIf(Reg, String), // if r3 > 0, jump.
    Call(String),        // pc = label
    Ret,                 // pc = back
    Syscall(Syscall),    // 0 = open, 1 = create, 2 = read, 3 = write
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
                    self.write(self.read_reg(reg), self.read(*val));
                    self.pc += 1
                }
                Instr::Store(val, reg) => {
                    self.write(self.read_reg(reg), *val);
                    self.pc += 1
                }
                Instr::Mul(r0, r1, r2) => {
                    self.write_reg(r0, self.read_reg(r1) * self.read_reg(r2));
                    self.pc += 1
                }
                Instr::Sub(r0, r1, r2) => {
                    self.write_reg(r0, self.read_reg(r1) - self.read_reg(r2));
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
                    for (i, reg) in [Reg::R1, Reg::R2, Reg::R3].iter().enumerate() {
                        let val = self.read_reg(reg);
                        let addr = self.sp.wrapping_add((i as u16) * 2);
                        self.write(addr, val);
                    }
                    self.sp = self.sp.wrapping_add(6);
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

                    self.sp = self.sp.wrapping_sub(6);
                    for (i, reg) in [Reg::R1, Reg::R2, Reg::R3].iter().enumerate() {
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
            }
        }
    }
}

// Working factorial example:
fn main() {
    let mut vm = VM::new();

    let prog = vec![
        Instr::Label("main".into()),    // main:
        Instr::Imm(Reg::R0, 5),         //   r0 = 5
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

    vm.run(&prog);
}
