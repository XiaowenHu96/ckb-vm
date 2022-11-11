use ckb_vm_definitions::instructions as insts;

use crate::new_decoder::def::RiscvOpcode;

use crate::instructions::utils::{
    btype_immediate, funct3, funct7, itype_immediate, jtype_immediate, opcode, rd, rs1, rs2,
    stype_immediate, utype_immediate,
};

use crate::instructions::{
    blank_instruction, set_instruction_length_4, Instruction, Itype, Register, Rtype, Stype, Utype,
};

// The FENCE instruction is used to order device I/O and memory accesses
// as viewed by other RISC- V harts and external devices or coprocessors.
#[derive(Debug, Clone, Copy)]
pub struct FenceType(Instruction);

impl FenceType {
    pub fn new(fm: u8, pred: u8, succ: u8) -> Self {
        FenceType(Rtype::new(insts::OP_FENCE, fm as usize, pred as usize, succ as usize).0)
    }

    pub fn fm(self) -> u8 {
        Rtype(self.0).rd() as u8
    }

    pub fn pred(self) -> u8 {
        Rtype(self.0).rs1() as u8
    }

    pub fn succ(self) -> u8 {
        Rtype(self.0).rs2() as u8
    }
}

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
    let inst = match opcode {
        RiscvOpcode::AUIPC | RiscvOpcode::LUI => Some(
            Utype::new_s(
                inst_code,
                rd(instruction_bits),
                utype_immediate(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::JAL => Some(
            Utype::new_s(
                inst_code,
                rd(instruction_bits),
                jtype_immediate(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::JALR => Some(
            Itype::new_s(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                itype_immediate(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::LB
        | RiscvOpcode::LH
        | RiscvOpcode::LW
        | RiscvOpcode::LBU
        | RiscvOpcode::LHU => Some(
            Itype::new_s(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                itype_immediate(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::LWU | RiscvOpcode::LD if rv64 => Some(
            Itype::new_s(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                itype_immediate(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::ADDI
        | RiscvOpcode::SLTI
        | RiscvOpcode::SLTIU
        | RiscvOpcode::XORI
        | RiscvOpcode::ORI
        | RiscvOpcode::ANDI => Some(
            Itype::new_s(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                itype_immediate(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::SLLI | RiscvOpcode::SRLI | RiscvOpcode::SRAI => Some(
            Itype::new_s(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                itype_immediate(instruction_bits) & i32::from(R::SHIFT_MASK),
            )
            .0,
        ),
        RiscvOpcode::BEQ
        | RiscvOpcode::BNE
        | RiscvOpcode::BLT
        | RiscvOpcode::BGE
        | RiscvOpcode::BLTU
        | RiscvOpcode::BGEU => Some(
            Stype::new_s(
                inst_code,
                btype_immediate(instruction_bits),
                rs1(instruction_bits),
                rs2(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::SB | RiscvOpcode::SH | RiscvOpcode::SW => Some(
            Stype::new_s(
                inst_code,
                stype_immediate(instruction_bits),
                rs1(instruction_bits),
                rs2(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::SD if rv64 => Some(
            Stype::new_s(
                inst_code,
                stype_immediate(instruction_bits),
                rs1(instruction_bits),
                rs2(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::ADD
        | RiscvOpcode::SUB
        | RiscvOpcode::SLL
        | RiscvOpcode::SLT
        | RiscvOpcode::SLTU
        | RiscvOpcode::XOR
        | RiscvOpcode::SRL
        | RiscvOpcode::SRA
        | RiscvOpcode::OR
        | RiscvOpcode::AND => Some(
            Rtype::new(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                rs2(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::FENCE if instruction_bits & 0x000_FFFFF == 0b_00000_000_00000_0001111 => Some(
            FenceType::new(
                ((instruction_bits & 0xF00_00000) >> 28) as u8,
                ((instruction_bits & 0x0F0_00000) >> 24) as u8,
                ((instruction_bits & 0x00F_00000) >> 20) as u8,
            )
            .0,
        ),
        RiscvOpcode::FENCEI if instruction_bits == 0b_0000_0000_0000_00000_001_00000_0001111 => {
            Some(blank_instruction(insts::OP_FENCEI))
        }
        RiscvOpcode::ECALL | RiscvOpcode::EBREAK => Some(blank_instruction(inst_code)),
        RiscvOpcode::ADDIW if rv64 => Some(
            Itype::new_s(
                insts::OP_ADDIW,
                rd(instruction_bits),
                rs1(instruction_bits),
                itype_immediate(instruction_bits),
            )
            .0,
        ),
        RiscvOpcode::SLLIW | RiscvOpcode::SRLIW | RiscvOpcode::SRAIW => Some(
            Itype::new_s(
                inst_code,
                rd(instruction_bits),
                rs1(instruction_bits),
                itype_immediate(instruction_bits) & 0x1F,
            )
            .0,
        ),
        RiscvOpcode::ADDW
        | RiscvOpcode::SUBW
        | RiscvOpcode::SLLW
        | RiscvOpcode::SRLW
        | RiscvOpcode::SRAW
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

pub fn nop() -> Instruction {
    Itype::new_u(insts::OP_ADDI, 0, 0, 0).0
}
