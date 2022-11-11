use ckb_vm_definitions::instructions as insts;

use crate::instructions::utils::{funct3, funct7, opcode, rd, rs1, rs2};
use crate::instructions::{set_instruction_length_4, Instruction, Register, Rtype};
use crate::new_decoder::def::RiscvOpcode;

pub fn factory<R: Register>(
    opcode: RiscvOpcode,
    inst_code: insts::InstructionOpcode,
    instruction_bits: u32,
    _: u32,
) -> Option<Instruction> {
    let bit_length = R::BITS;
    if bit_length != 32 && bit_length != 64 {
        return None;
    }
    let rv64 = bit_length == 64;
    if funct7(instruction_bits) != 0b_0000001 {
        return None;
    }
    let inst = match opcode {
        RiscvOpcode::MUL
        | RiscvOpcode::MULH
        | RiscvOpcode::MULHSU
        | RiscvOpcode::MULHU
        | RiscvOpcode::DIV
        | RiscvOpcode::DIVU
        | RiscvOpcode::REM
        | RiscvOpcode::REMU => Some(
            Rtype::new(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                rs2(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::MULW
        | RiscvOpcode::DIVW
        | RiscvOpcode::DIVUW
        | RiscvOpcode::REMW
        | RiscvOpcode::REMUW
            if rv64 =>
        {
            Some(
                Rtype::new(
                    inst_code,
                    rd(instruction_bits),
                    rs1(instruction_bits),
                    rs2(instruction_bits),
                )
                .0,
            )
        }
        _ => None,
    };
    inst.map(set_instruction_length_4)
}
