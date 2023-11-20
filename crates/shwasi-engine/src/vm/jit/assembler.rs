#![allow(dead_code)]

#[derive(Debug)]
pub struct Assembler {
    out: Vec<u8>,
}

impl Assembler {
    pub fn new() -> Self {
        Self { out: Vec::new() }
    }

    pub fn nop(&mut self) {
        self.push_u32(0xd503201f);
    }

    pub fn ret(&mut self) {
        self.push_u32(0xd65f03c0);
    }

    /// Return the assembled code, and add a ret instruction to the end.
    pub fn consume(mut self) -> Vec<u8> {
        self.ret();
        self.out
    }

    fn push_u32(&mut self, u: u32) {
        self.out.push((u & 0xff) as u8);
        self.out.push(((u >> 8) & 0xff) as u8);
        self.out.push(((u >> 16) & 0xff) as u8);
        self.out.push(((u >> 24) & 0xff) as u8);
    }
}
