use ckb_vm_definitions::instructions as insts;

pub struct RiscvInstructionInfo {
    mask: u32,
    match_bits: u32,
    opcode: RiscvOpcode,
    inst_code: insts::InstructionOpcode,
}

impl RiscvInstructionInfo {
    pub const fn new(
        mask: u32,
        match_bits: u32,
        opcode: RiscvOpcode,
        inst_code: insts::InstructionOpcode,
    ) -> Self {
        Self {
            mask,
            match_bits,
            opcode,
            inst_code,
        }
    }
}

macro_rules! declare_riscv_instruction {
    ( $( ($name:ident, $mask:literal, $match_bits:literal, $inst_code: ident) ),* ) => {
        #[derive(Clone, Copy)]
        pub enum RiscvOpcode {
            $($name,)*
        }
        $(
        const $name : RiscvInstructionInfo =
            RiscvInstructionInfo::new($mask, $match_bits, RiscvOpcode::$name, insts::$inst_code);
        )*
    }
}

macro_rules! instruction_factory {
    ($group_name:ident, ($( $name:ident ),* ) ) => {
        pub struct $group_name {
            instructions: Vec<RiscvInstructionInfo>,
        }
        impl $group_name {
            pub fn new() -> Self {
                Self {instructions : vec![$($name,)*],
                }
            }

            pub fn decode(& self, bits: u32) -> Option<(RiscvOpcode, insts::InstructionOpcode)> {
                for i in &self.instructions {
                    if (i.mask & bits) == i.match_bits {
                        return Some((i.opcode, i.inst_code))
                    }
                }
                None
            }

            pub fn get(&self, idx:usize) -> u32 {
                return self.instructions[idx % self.instructions.len()].match_bits;
            }
        }
    }
}

instruction_factory!(
    RVIFactory,
    (
        ADD, ADDI, ADDIW, ADDW, AND, ANDI, AUIPC, BEQ, BGE, BGEU, BLT, BLTU, BNE, EBREAK, ECALL,
        FENCE, FENCEI, JAL, JALR, LB, LBU, LD, LH, LHU, LUI, LW, LWU, OR, ORI, SB, SD, SH, SLL,
        SLLI, SLLIW, SLLW, SLT, SLTI, SLTIU, SLTU, SRA, SRAI, SRAIW, SRAW, SRL, SRLI, SRLIW, SRLW,
        SUB, SUBW, SW, XOR, XORI
    )
);

instruction_factory!(
    RVMFactory,
    (DIV, DIVU, DIVUW, DIVW, MUL, MULH, MULHSU, MULHU, MULW, REM, REMU, REMUW, REMW)
);

instruction_factory!(
    RVCFactory,
    (
        CADD, CADDI, CADDI16SP, CADDI4SPN, CADDIW, CADDW, CAND, CANDI, CBEQZ, CBNEZ, CEBREAK, CFLD,
        CFLDSP, CFLW, CFLWSP, CFSD, CFSDSP, CFSW, CFSWSP, CJ, CJAL, CJALR, CJR, CLD, CLDSP, CLI,
        CLUI, CLW, CLWSP, CMV, CNOP, COR, CSD, CSDSP, CSLLI, CSRAI, CSRLI, CSUB, CSUBW, CSW, CSWSP,
        CXOR
    )
);

// #[rustfmt::skip]
declare_riscv_instruction!(
    // RVM
    (DIV, 0xfe00707f, 0x2004033, OP_DIV),
    (DIVU, 0xfe00707f, 0x2005033, OP_DIVU),
    (DIVUW, 0xfe00707f, 0x200503b, OP_DIVUW),
    (DIVW, 0xfe00707f, 0x200403b, OP_DIVW),
    (MUL, 0xfe00707f, 0x2000033, OP_MUL),
    (MULH, 0xfe00707f, 0x2001033, OP_MULH),
    (MULHSU, 0xfe00707f, 0x2002033, OP_MULHSU),
    (MULHU, 0xfe00707f, 0x2003033, OP_MULHU),
    (MULW, 0xfe00707f, 0x200003b, OP_MULW),
    (REM, 0xfe00707f, 0x2006033, OP_REM),
    (REMU, 0xfe00707f, 0x2007033, OP_REMU),
    (REMUW, 0xfe00707f, 0x200703b, OP_REMUW),
    (REMW, 0xfe00707f, 0x200603b, OP_REMW),
    // RVI
    (ADD, 0xfe00707f, 0x33, OP_ADD),
    (ADDI, 0x707f, 0x13, OP_ADDI),
    (ADDIW, 0x707f, 0x1b, OP_ADDIW),
    (ADDW, 0xfe00707f, 0x3b, OP_ADDW),
    (AND, 0xfe00707f, 0x7033, OP_AND),
    (ANDI, 0x707f, 0x7013, OP_ANDI),
    (AUIPC, 0x7f, 0x17, OP_AUIPC),
    (BEQ, 0x707f, 0x63, OP_BEQ),
    (BGE, 0x707f, 0x5063, OP_BGE),
    (BGEU, 0x707f, 0x7063, OP_BGEU),
    (BLT, 0x707f, 0x4063, OP_BLT),
    (BLTU, 0x707f, 0x6063, OP_BLTU),
    (BNE, 0x707f, 0x1063, OP_BNE),
    (EBREAK, 0xffffffff, 0x100073, OP_EBREAK),
    (ECALL, 0xffffffff, 0x73, OP_ECALL),
    (FENCE, 0x707f, 0xf, OP_FENCE),
    (FENCEI, 0x707f, 0x100f, OP_FENCEI),
    (JAL, 0x7f, 0x6f, OP_JAL),
    (JALR, 0x707f, 0x67, OP_JALR),
    (LB, 0x707f, 0x3, OP_LB),
    (LBU, 0x707f, 0x4003, OP_LBU),
    (LD, 0x707f, 0x3003, OP_LD),
    (LH, 0x707f, 0x1003, OP_LH),
    (LHU, 0x707f, 0x5003, OP_LHU),
    (LUI, 0x7f, 0x37, OP_LUI),
    (LW, 0x707f, 0x2003, OP_LW),
    (LWU, 0x707f, 0x6003, OP_LWU),
    (OR, 0xfe00707f, 0x6033, OP_OR),
    (ORI, 0x707f, 0x6013, OP_ORI),
    (SB, 0x707f, 0x23, OP_SB),
    (SD, 0x707f, 0x3023, OP_SD),
    (SH, 0x707f, 0x1023, OP_SH),
    (SLL, 0xfe00707f, 0x1033, OP_SLL),
    (SLLI, 0xfc00707f, 0x1013, OP_SLLI),
    (SLLIW, 0xfe00707f, 0x101b, OP_SLLIW),
    (SLLW, 0xfe00707f, 0x103b, OP_SLLW),
    (SLT, 0xfe00707f, 0x2033, OP_SLT),
    (SLTI, 0x707f, 0x2013, OP_SLTI),
    (SLTIU, 0x707f, 0x3013, OP_SLTIU),
    (SLTU, 0xfe00707f, 0x3033, OP_SLTU),
    (SRA, 0xfe00707f, 0x40005033, OP_SRA),
    (SRAI, 0xfc00707f, 0x40005013, OP_SRAI),
    (SRAIW, 0xfe00707f, 0x4000501b, OP_SRAIW),
    (SRAW, 0xfe00707f, 0x4000503b, OP_SRAW),
    (SRL, 0xfe00707f, 0x5033, OP_SRL),
    (SRLI, 0xfc00707f, 0x5013, OP_SRLI),
    (SRLIW, 0xfe00707f, 0x501b, OP_SRLIW),
    (SRLW, 0xfe00707f, 0x503b, OP_SRLW),
    (SUB, 0xfe00707f, 0x40000033, OP_SUB),
    (SUBW, 0xfe00707f, 0x4000003b, OP_SUBW),
    (SW, 0x707f, 0x2023, OP_SW),
    (XOR, 0xfe00707f, 0x4033, OP_XOR),
    (XORI, 0x707f, 0x4013, OP_XORI),
    // RVC:
    (CADD, 0xf003, 0x9002, OP_ADD),
    (CADDI, 0xe003, 0x1, OP_ADD),
    (CADDI16SP, 0xef83, 0x6101, OP_ADD),
    (CADDI4SPN, 0xe003, 0x0, OP_ADD),
    (CADDIW, 0xe003, 0x2001, OP_ADD),
    (CADDW, 0xfc63, 0x9c21, OP_ADD),
    (CAND, 0xfc63, 0x8c61, OP_ADD),
    (CANDI, 0xec03, 0x8801, OP_ADD),
    (CBEQZ, 0xe003, 0xc001, OP_ADD),
    (CBNEZ, 0xe003, 0xe001, OP_ADD),
    (CEBREAK, 0xffff, 0x9002, OP_ADD),
    (CFLD, 0xe003, 0x2000, OP_ADD),
    (CFLDSP, 0xe003, 0x2002, OP_ADD),
    (CFLW, 0xe003, 0x6000, OP_ADD),
    (CFLWSP, 0xe003, 0x6002, OP_ADD),
    (CFSD, 0xe003, 0xa000, OP_ADD),
    (CFSDSP, 0xe003, 0xa002, OP_ADD),
    (CFSW, 0xe003, 0xe000, OP_ADD),
    (CFSWSP, 0xe003, 0xe002, OP_ADD),
    (CJ, 0xe003, 0xa001, OP_ADD),
    (CJAL, 0xe003, 0x2001, OP_ADD),
    (CJALR, 0xf07f, 0x9002, OP_ADD),
    (CJR, 0xf07f, 0x8002, OP_ADD),
    (CLD, 0xe003, 0x6000, OP_ADD),
    (CLDSP, 0xe003, 0x6002, OP_ADD),
    (CLI, 0xe003, 0x4001, OP_ADD),
    (CLUI, 0xe003, 0x6001, OP_ADD),
    (CLW, 0xe003, 0x4000, OP_ADD),
    (CLWSP, 0xe003, 0x4002, OP_ADD),
    (CMV, 0xf003, 0x8002, OP_ADD),
    (CNOP, 0xef83, 0x1, OP_ADD),
    (COR, 0xfc63, 0x8c41, OP_ADD),
    (CSD, 0xe003, 0xe000, OP_ADD),
    (CSDSP, 0xe003, 0xe002, OP_ADD),
    (CSLLI, 0xe003, 0x2, OP_ADD),
    (CSRAI, 0xec03, 0x8401, OP_ADD),
    (CSRLI, 0xec03, 0x8001, OP_ADD),
    (CSUB, 0xfc63, 0x8c01, OP_ADD),
    (CSUBW, 0xfc63, 0x9c01, OP_ADD),
    (CSW, 0xe003, 0xc000, OP_ADD),
    (CSWSP, 0xe003, 0xc002, OP_ADD),
    (CXOR, 0xfc63, 0x8c21, OP_ADD)
);
