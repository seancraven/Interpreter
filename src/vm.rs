use std::{array, fmt::Debug};
use tracing::{Level, info, instrument, span};

use crate::{
    code::{ByteCode, Instructions, Op},
    object::Object,
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
        if self.pointer == 0 {
            return Err(anyhow::anyhow!("Trying to set negative pointer"));
        };
        self.pointer -= 1;
        Ok(self.stack[self.pointer].clone())
    }

    #[instrument(skip_all)]
    pub fn run(&mut self) -> Result<()> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Op::from(self.instructions[ip]);
            let span = span!(Level::INFO, "Instruction Parse", op = format!("{:?}", op));
            let _guard = span.enter();
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
                Op::Add | Op::Sub | Op::Div | Op::Mul => {
                    let right = self
                        .pop()
                        .context("Failure during addition popping value from stack.")?;
                    let left = self
                        .pop()
                        .context("Failure during addition popping value from stack.")?;
                    let result = op.apply_pairwise_on_ints(
                        left.get_int().unwrap(),
                        right.get_int().unwrap(),
                    )?;
                    self.push(Object::Int(result))
                        .context("Pushing to stack after addition failed")?;
                    ip += 1;
                }
                Op::Pop => {
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
    use crate::{
        code::disassemble, compiler::Compiler, monitoring::init_tracing, object::Object, parser,
    };
    use anyhow::Context;
    use tracing::info;

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
        println!("{}", "=".repeat(80));
        println!("{}", "=".repeat(80));
        let program = parser::Parser::parse(test_case.input)
            .context(format!("{:?} parsing failed", test_case.input))?;
        let bytecode = Compiler::new().compile(program).unwrap();
        let human_readable_instructions = disassemble(&bytecode.instructions);
        let mut vm = Vm::new(bytecode);
        vm.run().unwrap();
        let object = vm.last_popped_elememnt().unwrap();
        assert_eq!(
            object, test_case.expected_object,
            "Instructions\n{}",
            human_readable_instructions
        );
        println!("{}", "=".repeat(80));
        println!("{}", "=".repeat(80));
        Ok(())
    }

    #[test]
    fn test_vm() -> anyhow::Result<()> {
        let table = vec![
            TestCase::new("1 + 2", Object::Int(3)),
            TestCase::new("1 + 4", Object::Int(5)),
            TestCase::new("1 * 4", Object::Int(4)),
            TestCase::new("1 * 4 * 5", Object::Int(20)),
            TestCase::new("1 - 4 * 5", Object::Int(-19)),
            TestCase::new("1 - 4", Object::Int(-3)),
            TestCase::new("4 / 1", Object::Int(4)),
        ];
        init_tracing();
        for test in table {
            info!("Test Case {}", test.input);
            run_test(test.clone())?;
        }
        Ok(())
    }
}
