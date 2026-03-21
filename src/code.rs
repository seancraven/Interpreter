// Fist part of the compiler book defining opcodes.
// Goal is to implement Simple VM and Compiler to compile
// 1  + 2 and generate the constant 3.

use anyhow::Context;
use tracing::instrument;

use crate::object::Object;
// Operation in the compiler is one byte wide
pub type OpCode = u8;
// Operation + it's operands forms a full instruction .
// Note atm this is a huge object ot store a limited number of bytes in.
pub type Instructions = Vec<u8>;
// The whole program is just a sequence of Instructions, all our VM will do
// is to iterate through this.
#[derive(Debug, Clone)]
pub struct ByteCode {
    pub constants: Vec<Object>,
    pub instructions: Instructions,
}
impl ByteCode {
    pub fn new(constants: Vec<Object>, instructions: Vec<u8>) -> ByteCode {
        return ByteCode {
            constants,
            instructions,
        };
    }
}

/// This doesn't actually need to allocate the vector here.
/// We could just make an iterator.
pub fn read_operands(instructions: &[u8], widths: &[usize]) -> Vec<usize> {
    let mut out = Vec::with_capacity(widths.len());
    let mut current_index = 0;

    for width in widths.iter() {
        let end_index = current_index + width;
        let big_endian_bytes = &instructions[current_index..end_index];
        let value = match width {
            1 => u8::from_be_bytes(*big_endian_bytes.as_array::<1>().context("..").expect(""))
                as usize,
            2 => u16::from_be_bytes(*big_endian_bytes.as_array::<2>().context("..").expect(""))
                as usize,
            0 => continue,
            _ => panic!("Unsupported number of bytes for an instruction."),
        };
        out.push(value);
        current_index = end_index;
    }
    out
}

pub fn disassemble(instructions: &[u8]) -> String {
    let mut current_index = 0;
    let mut buf = String::new();
    while current_index < instructions.len() {
        let op = Op::from(instructions[current_index]);
        let def = op.definitions();
        buf.push_str(def.name);
        buf.push(' ');

        let end_index = current_index + def.instruction_length();
        let operation_instructions = &instructions[current_index + 1..end_index];
        let values = read_operands(operation_instructions, &def.operand_widths);
        let values_string = values
            .iter()
            .map(|v| v.to_string())
            .reduce(|mut acc, i| {
                acc.push_str(&i);
                acc
            })
            .unwrap_or_else(String::new);
        buf.push_str(&values_string);
        buf.push('\n');
        current_index = end_index;
    }
    if buf.is_empty() {
        return buf;
    }
    buf
}

// Types of operations represented by the opcode.
#[derive(Clone, Copy, Debug)]
pub enum Op {
    Constant,
    Add,
    Mul,
    Div,
    Sub,
    Pop,
}

impl Op {
    /// Constant value lookup table for the definition of operation, this shouldn't even be computed.
    pub fn definitions(&self) -> Definition {
        match *self {
            // We don't expect to store more than 6356 values u16
            // is large enough to index our constant array.
            Op::Constant => Definition::new("Constant", vec![2]),
            Op::Pop => Definition::new("Pop", vec![]),
            Op::Add => Definition::new("Add", vec![]),
            Op::Sub => Definition::new("Sub", vec![]),
            Op::Mul => Definition::new("Mul", vec![]),
            Op::Div => Definition::new("Div", vec![]),
        }
    }
    #[instrument]
    pub fn apply_pairwise_on_ints(&self, a: isize, b: isize) -> anyhow::Result<isize> {
        match *self {
            Op::Add => Ok(a + b),
            Op::Sub => Ok(a - b),
            Op::Mul => Ok(a * b),
            Op::Div => Ok(a / b),
            _ => Err(anyhow::anyhow!("Invalid operator for integers.")),
        }
    }
}
impl From<Op> for u8 {
    fn from(value: Op) -> Self {
        match value {
            Op::Constant => 0,
            Op::Pop => 1,
            Op::Add => 2,
            Op::Sub => 3,
            Op::Mul => 4,
            Op::Div => 5,
        }
    }
}
impl From<u8> for Op {
    fn from(value: u8) -> Self {
        match value {
            0 => Op::Constant,
            1 => Op::Pop,
            2 => Op::Add,
            3 => Op::Sub,
            4 => Op::Mul,
            5 => Op::Div,
            _ => panic!("{:?} Invalid opcode.", value),
        }
    }
}

/// Structure describing important information about an
/// opcode.
/// name is the name of the code, and operand_widths, is an
/// array of lenght the number of operands, and integer values, about how many bytes
/// each operand can be.
#[derive(Debug)]
pub struct Definition {
    pub name: &'static str,
    operand_widths: Vec<usize>,
}
impl Definition {
    pub fn new(name: &'static str, operand_widths: Vec<usize>) -> Self {
        Self {
            name,
            operand_widths,
        }
    }
    pub fn instruction_length(&self) -> usize {
        1 + self.operand_widths.iter().map(|v| *v).sum::<usize>()
    }
}

pub fn make(op: Op, operands: &[usize]) -> Instructions {
    let def = op.definitions();
    let operation_byte: u8 = op.into();
    let mut instruction: Vec<u8> = Vec::with_capacity(def.instruction_length());
    instruction.push(operation_byte);
    // Iterate through the operands, turn them into byte array's.
    // Depending on the type of instruction, we have different expected widths of the operand,
    // Making the operands decoding into bytes a question
    for (operand, width) in operands.iter().zip(def.operand_widths) {
        let be_bytes = match width {
            2 => (*operand as u16).to_be_bytes(),
            0 => continue,
            _ => panic!("Invalid byte width, values are 1,2,4"),
        };
        instruction.extend(be_bytes);
    }
    return instruction;
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    #[test]
    fn test_make() -> Result<()> {
        let table: Vec<(Op, Vec<usize>, Vec<u8>)> = vec![
            (
                Op::Constant,
                vec![65534],
                vec![u8::from(Op::Constant), 255, 254],
            ),
            (Op::Add, vec![], vec![u8::from(Op::Add)]),
            (Op::Pop, vec![], vec![u8::from(Op::Pop)]),
        ];
        for (op, operand, expected_instructions) in &table {
            let instructions = make(*op, operand);
            assert_eq!(
                instructions.len(),
                expected_instructions.len(),
                "Instructions sets should be the same length got {:?} expected {:?}.",
                disassemble(&instructions),
                disassemble(expected_instructions)
            );
            for (i, expected_i) in instructions.iter().zip(expected_instructions.iter()) {
                assert_eq!(i, expected_i, "Big Endian bytes should be equal");
            }
        }
        Ok(())
    }

    #[test]
    fn test_disassemble() -> Result<()> {
        let bytes: Vec<u8> = vec![2, 0, 255, 254];
        let dis = disassemble(&bytes);
        let expected = "Add \nConstant 65534\n";
        assert_eq!(dis, expected);
        Ok(())
    }
}
