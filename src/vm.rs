use std::array;

use crate::{
    code::{ByteCode, Instructions, Op},
    object::Object,
    vm,
};
use anyhow::{Context, Result};

// Maximum stack size in the
const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: [Object; STACK_SIZE],

    pointer: usize,
}

impl Vm {
    pub fn new(bytecode: ByteCode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: array::repeat(Object::Null),
            pointer: 0,
        }
    }
    pub fn push(&mut self, obj: Object) -> Result<()> {
        if self.pointer > STACK_SIZE {
            return Err(anyhow::anyhow!("Stack overflow"));
        }
        self.stack[self.pointer] = obj;
        self.pointer += 1;
        Ok(())
    }
    pub fn pop(&mut self) -> Result<Object> {
        println!("Call to pop");
        if self.pointer == 0 {
            return Err(anyhow::anyhow!("Trying to set negative pointer"));
        };
        self.pointer -= 1;
        Ok(self.stack[self.pointer].clone())
    }
    pub fn run(&mut self) -> Result<()> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Op::from(self.instructions[ip]);
            // NOTE: Edjit you must increase the pointer.
            match op {
                Op::Constant => {
                    let byte = self.instructions[ip + 1];
                    let next_byte = self.instructions[ip + 2];
                    let constant_index = u16::from_be_bytes([byte, next_byte]);
                    let object = self.constants[constant_index as usize].clone();
                    ip += 3;
                    self.push(object)?;
                }
                Op::Add => {
                    println!("Popping because of add");
                    let left = self
                        .pop()
                        .context("Failure during addition popping value from stack.")?;
                    let right = self
                        .pop()
                        .context("Failure during addition popping value from stack.")?;
                    let result = left.get_int().unwrap() + right.get_int().unwrap();
                    self.push(Object::Int(result))
                        .context("Pushing to stack after addition failed")?;
                    ip += 1;
                }
                Op::Pop => {
                    println!("Popping because of pop");
                    self.pop().context("Pop operation failed.")?;
                    ip += 1;
                }
            }
        }
        Ok(())
    }
    pub fn last_popped_elememnt(&mut self) -> Option<Object> {
        self.stack.get(self.pointer).map(|o| o.clone())
    }
    pub fn stack_top(&mut self) -> Option<Object> {
        if self.pointer == 0 {
            return None;
        }
        Some(self.stack[self.pointer - 1].clone())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{code::disassemble, compiler::Compiler, object::Object, parser};
    use anyhow::Context;

    #[derive(Debug, Clone)]
    struct TestCase {
        input: &'static str,
        expected_object: Object,
    }

    impl TestCase {
        fn new(input: &'static str, expected_object: Object) -> Self {
            Self {
                input,
                expected_object,
            }
        }
    }

    fn run_test(test_case: TestCase) -> anyhow::Result<()> {
        let program = parser::Parser::parse(test_case.input)
            .context(format!("{:?} parsing failed", test_case.input))?;
        let bytecode = Compiler::new().compile(program).unwrap();
        println!("Instructions\n {}", disassemble(&bytecode.instructions));
        let mut vm = Vm::new(bytecode);
        vm.run().unwrap();
        let object = vm.last_popped_elememnt().unwrap();
        assert_eq!(object, test_case.expected_object);

        Ok(())
    }

    #[test]
    fn test_vm() -> anyhow::Result<()> {
        let table = vec![
            TestCase::new("1 + 2", Object::Int(3)),
            TestCase::new("1 + 4", Object::Int(5)),
        ];
        for test in table {
            run_test(test.clone())?;
        }
        Ok(())
    }
}
