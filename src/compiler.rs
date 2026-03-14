use crate::{
    ast::Node,
    code::{ByteCode, Instructions, Op, make},
    object::Object,
};
use anyhow::Result;

#[derive(Debug)]
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}
impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            instructions: vec![],
            constants: vec![],
        }
    }
    pub fn add_constant(&mut self, o: Object) -> usize {
        self.constants.push(o);
        self.constants.len() - 1
    }

    pub fn compile(&mut self, node: impl Node) -> Result<ByteCode> {
        node.add_bytecode_to_compiler(self)?;
        Ok(self.bytecode())
    }
    pub fn add_instructions(&mut self, instructions: &[u8]) -> usize {
        let new_instruction_start_pos = self.instructions.len();
        self.instructions.extend_from_slice(instructions);
        new_instruction_start_pos
    }
    /// Mutates compiler to add bytecode for a given operation.
    pub fn emit_bytecode(&mut self, op: Op, operands: &[usize]) -> usize {
        let instructions = make(op, operands);
        let new_instruction_start_pos = self.add_instructions(&instructions);
        new_instruction_start_pos
    }
    pub fn bytecode(&mut self) -> ByteCode {
        return ByteCode::new(self.constants.clone(), self.instructions.clone());
    }
}

#[cfg(test)]
mod test {
    use anyhow::Context;

    use super::*;
    use crate::{
        code::{Instructions, Op, disassemble, make},
        object::Object,
        parser,
    };

    #[derive(Debug, Clone)]
    struct TestCase {
        input: &'static str,
        expected_constants: Vec<Object>,
        expected_instructions: Vec<Instructions>,
    }

    impl TestCase {
        fn new(
            input: &'static str,
            expected_constants: Vec<Object>,
            expected_instructions: Vec<Instructions>,
        ) -> Self {
            Self {
                input,
                expected_constants,
                expected_instructions,
            }
        }

        fn expected_instructions(&self) -> Vec<u8> {
            self.expected_instructions
                .clone()
                .into_iter()
                .flatten()
                .collect()
        }
    }

    fn run_test(test_case: TestCase) -> anyhow::Result<()> {
        let program = parser::Parser::parse(test_case.input)
            .context(format!("{:?} parsing failed", test_case.input))?;
        let bytecode = Compiler::new()
            .compile(program)
            .context(format!("{:?} compiling failed", test_case.input))?;

        assert_eq!(
            bytecode.instructions,
            test_case.expected_instructions(),
            "Instructions aren't equal {:?}, expected {:?} got {:?}",
            test_case.input,
            disassemble(&test_case.expected_instructions()),
            disassemble(&bytecode.instructions),
        );
        assert_eq!(
            bytecode.constants, test_case.expected_constants,
            "Constant's aren't equal {:?}",
            test_case.input
        );
        Ok(())
    }

    #[test]
    fn test_compiler() -> anyhow::Result<()> {
        let table = vec![
            TestCase::new(
                "1 + 2",
                vec![Object::Int(1), Object::Int(2)],
                // NOTE: The operand is the expected index of the constant.
                vec![
                    make(Op::Constant, &[0]),
                    make(Op::Constant, &[1]),
                    make(Op::Add, &[]),
                    make(Op::Pop, &[]),
                ],
            ),
            TestCase::new(
                "1; 2",
                vec![Object::Int(1), Object::Int(2)],
                vec![
                    make(Op::Constant, &[0]),
                    make(Op::Pop, &[]),
                    make(Op::Constant, &[1]),
                    make(Op::Pop, &[]),
                ],
            ),
        ];

        for test in table {
            run_test(test.clone())?;
        }
        Ok(())
    }
}
