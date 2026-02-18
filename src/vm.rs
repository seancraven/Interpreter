use crate::{
    code::{ByteCode, Instructions, Op},
    object::Object,
};
use anyhow::Result;

// Maximum stack size in the
const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    pointer: usize,
}

impl Vm {
    pub fn new(bytecode: ByteCode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            pointer: 0,
        }
    }
    pub fn push(&mut self, obj: Object) -> Result<()> {
        if self.pointer > STACK_SIZE {
            return Err(anyhow::anyhow!("Stack overflow"));
        }
        self.stack.push(obj);
        self.pointer += 1;
        Ok(())
    }
    pub fn run(&mut self) -> Result<()> {
        let mut ip = 0;
        println!("Instruction lenght {}", self.instructions.len());
        while ip < self.instructions.len() {
            let op = Op::from(self.instructions[ip]);
            match op {
                Op::Constant => {
                    let byte = self.instructions[ip + 1];
                    let next_byte = self.instructions[ip + 2];
                    let constant_index = u16::from_be_bytes([byte, next_byte]);
                    let object = self.constants[constant_index as usize].clone();
                    ip += 3;
                    self.push(object)?;
                }
            }
        }
        Ok(())
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
    use crate::{compiler::Compiler, object::Object, parser};
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
        let mut vm = Vm::new(bytecode);
        vm.run().unwrap();
        let object = vm.stack_top().unwrap();
        assert_eq!(object, test_case.expected_object);

        Ok(())
    }

    #[test]
    fn test_vm() -> anyhow::Result<()> {
        let table = vec![
            TestCase::new("1 + 2", Object::Int(2)),
            TestCase::new("1 + 4", Object::Int(4)),
        ];
        for test in table {
            run_test(test.clone())?;
        }
        Ok(())
    }
}
