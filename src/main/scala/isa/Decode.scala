package isa

import chisel3._ 
import chisel3.util._ 
import conf.Config
import ISA._
import MicroOpCtrl._
import icore.InstType

class Mops extends Bundle with Config with InstType {
//  val inst_type     = UInt(SZ_INST_TYPE.W)
  val next_pc       = UInt(SZ_NEXT_PC.W)
  val alu_mdu_lsu   = UInt(typeLen.W)
  val branch_type   = UInt(SZ_BR_TYPE.W)
  val src_a         = UInt(SZ_ALU_A.W)
  val src_b         = UInt(SZ_ALU_B.W)
  val write_dest    = UInt(SZ_W_DEST.W)
  val alu_op        = UInt(SZ_ALU_TYPE.W)
  val mem_width     = UInt(SZ_MEM_TYPE.W)
  val write_src     = UInt(SZ_WB_SRC.W)
  val rs1           = UInt(SZ_REG_ADDR.W)
  val rs2           = UInt(SZ_REG_ADDR.W)
  val rd            = UInt(SZ_REG_ADDR.W)  
  val imm           = UInt(len.W)
  val pc            = UInt(len.W)
}

class DecIO extends Bundle with Config {
  val pc   = Input(UInt(len.W))
  val inst = Input(UInt(len.W))
  val mops = Output(new Mops)
}

class Dec extends Module with InstType {
  val io = IO(new DecIO)

  val SHAMT = io.inst(10,  6)
  val IMM   = io.inst(15,  0)
  val IRS   = io.inst(25, 21)
  val IRT   = io.inst(20, 16)
  val IRD   = io.inst(15, 11)
  val IRA   = 31.U(SZ_REG_ADDR.W)
  val IXX   = 0.U(SZ_REG_ADDR.W)
  val UIMM  = Cat(Fill(16, 0.U), IMM)
  val SIMM  = Cat(Fill(16, IMM(15)), IMM)
  val JOffset = Cat(Fill(6, 0.U), io.inst(25, 0))

  // Decode
  val control_signal = ListLookup(io.inst,
                    List(Illegal ,  PC4     ,  toALU.U,   BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX, UIMM),
    Array(         /*      Inst  |   PC     | use      | Branch   |   A     |   B     |  D      | alu        |  Mem    |     wb     | rs1 | rs2 |  rd | imm32 */
                   /*      Type  | Select   | which    | Type     | use rs1 | use rs2 | write   | Type       | Type    |   Select   |     |     |     |       */
                   /*  Structure | NextPC   | al md ls | Brch/Jmp | alusrcA | alusrcB | target  | alu OP     | B/H/W   | MultiIssue |     |     |     |       */
      NOP        -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX , UIMM),
      ADD        -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      ADDI       -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT , SIMM),
      ADDU       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAddu.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      ADDIU      -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAddu.U  , MemXXX  ,  WBALU     , IRS , IXX , IRT , SIMM),
      SUB        -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSub.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      SUBU       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSub.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      SLT        -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSlt.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      SLTI       -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluSlt.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT , SIMM),
      SLTU       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSltu.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      SLTIU      -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluSltu.U  , MemXXX  ,  WBALU     , IRS , IXX , IRT , SIMM),
      DIV        -> List(RType   ,  PC4     ,  toMDU.U,   BrXXX   ,  AReg   ,  BReg   ,  DHiLo  , MDU_DIV.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      DIVU       -> List(RType   ,  PC4     ,  toMDU.U,   BrXXX   ,  AReg   ,  BReg   ,  DHiLo  , MDU_DIVU.U , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      MULT       -> List(RType   ,  PC4     ,  toMDU.U,   BrXXX   ,  AReg   ,  BReg   ,  DHiLo  , MDU_MUL.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      MULTU      -> List(RType   ,  PC4     ,  toMDU.U,   BrXXX   ,  AReg   ,  BReg   ,  DHiLo  , MDU_MULU.U , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
           
      AND        -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAnd.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      ANDI       -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAnd.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT , SIMM),
      LUI        -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluLui.U   , MemXXX  ,  WBALU     , IXX , IXX , IRT , UIMM),
      NOR        -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluNor.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      OR         -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluOr.U    , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      ORI        -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluOr.U    , MemXXX  ,  WBALU     , IRS , IXX , IRT , SIMM),
      XOR        -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluXor.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      XORI       -> List(IType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluXor.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT , SIMM),
           
      SLLV       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSll.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      SLL        -> List(RSType  ,  PC4     ,  toAMU.U,   BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSll.U   , MemXXX  ,  WBALU     , IXX , IRT , IRD , UIMM),
      SRAV       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSra.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      SRA        -> List(RSType  ,  PC4     ,  toAMU.U,   BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSra.U   , MemXXX  ,  WBALU     , IXX , IRT , IRD , UIMM),
      SRLV       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSrl.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD , UIMM),
      SRL        -> List(RSType  ,  PC4     ,  toAMU.U,   BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSrl.U   , MemXXX  ,  WBALU     , IXX , IRT , IRD , UIMM),
           
      BEQ        -> List(IBType  ,  Branch  ,  toALU.U,   BrEQ    ,  AReg   ,  BReg   ,  DXXX   , aluSub.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX , SIMM),
      BNE        -> List(IBType  ,  Branch  ,  toALU.U,   BrNE    ,  AReg   ,  BReg   ,  DXXX   , aluSub.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX , SIMM),
      BGEZ       -> List(IBType  ,  Branch  ,  toALU.U,   BrGE    ,  AReg   ,  BXXX   ,  DXXX   , aluSub.U   , MemXXX  ,  WBXXX     , IRS , IXX , IXX , SIMM),
      BGTZ       -> List(IBType  ,  Branch  ,  toALU.U,   BrGT    ,  AReg   ,  BXXX   ,  DXXX   , aluSub.U   , MemXXX  ,  WBXXX     , IRS , IXX , IXX , SIMM),
      BLEZ       -> List(IBType  ,  Branch  ,  toALU.U,   BrLE    ,  AReg   ,  BXXX   ,  DXXX   , aluSub.U   , MemXXX  ,  WBXXX     , IRS , IXX , IXX , SIMM),
      BLTZ       -> List(IBType  ,  Branch  ,  toALU.U,   BrLT    ,  AReg   ,  BXXX   ,  DXXX   , aluSub.U   , MemXXX  ,  WBXXX     , IRS , IXX , IXX , SIMM),
      BGEZAL     -> List(IBType  ,  Branch  ,  toALU.U,   BrGE    ,  AReg   ,  BXXX   ,  DReg   , aluSub.U   , MemXXX  ,  WBALU     , IRS , IXX , IRA , SIMM),
      BLTZAL     -> List(IBType  ,  Branch  ,  toALU.U,   BrLT    ,  AReg   ,  BXXX   ,  DReg   , aluSub.U   , MemXXX  ,  WBALU     , IRS , IXX , IRA , SIMM),
           
      J          -> List(JType   ,  Jump    ,  toALU.U,   BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX , JOffset),
      JAL        -> List(JType   ,  Jump    ,  toALU.U,   BrXXX   ,  AXXX   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBPC      , IXX , IXX , IRA , JOffset),
      JR         -> List(JRType  ,  PCReg   ,  toALU.U,   BrXXX   ,  AReg   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IXX , IRD , UIMM),
      JALR       -> List(JRType  ,  PCReg   ,  toALU.U,   BrXXX   ,  AReg   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBPC      , IRS , IXX , IRA , UIMM),
           
      MFHI       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AHi    ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IXX , IXX , IRD , UIMM),
      MFLO       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  ALo    ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IXX , IXX , IRD , UIMM),
      MTHI       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BXXX   ,  DHi    , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IXX , IXX , UIMM),
      MTLO       -> List(RType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BXXX   ,  DLo    , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IXX , IXX , UIMM),
           
      BREAK      -> List(RTType  ,  Trap    ,  toAMU.U,   BrXXX   ,  AXXX   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX , UIMM),
      SYSCALL    -> List(RTType  ,  Trap    ,  toAMU.U,   BrXXX   ,  AXXX   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX , UIMM),
           
      LB         -> List(IMType  ,  PC4     ,  toLU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemByte ,  WBMEM     , IRS , IXX , IRT , SIMM),
      LBU        -> List(IMType  ,  PC4     ,  toLU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemByteU,  WBMEM     , IRS , IXX , IRT , SIMM),
      LH         -> List(IMType  ,  PC4     ,  toLU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemHalf ,  WBMEM     , IRS , IXX , IRT , SIMM),
      LHU        -> List(IMType  ,  PC4     ,  toLU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemHalfU,  WBMEM     , IRS , IXX , IRT , SIMM),
      LW         -> List(IMType  ,  PC4     ,  toLU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemWord ,  WBMEM     , IRS , IXX , IRT , SIMM),
      SB         -> List(IMType  ,  PC4     ,  toSU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd.U   , MemByte ,  WBXXX     , IRS , IRT , IXX , SIMM),
      SH         -> List(IMType  ,  PC4     ,  toSU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd.U   , MemHalf ,  WBXXX     , IRS , IRT , IXX , SIMM),
      SW         -> List(IMType  ,  PC4     ,  toSU.U ,   BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd.U   , MemWord ,  WBXXX     , IRS , IRT , IXX , SIMM),
           
      ERET       -> List(SType   ,  Ret     ,  toAMU.U,   BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX , UIMM),
      MFC0       -> List(SType   ,  PC4     ,  toAMU.U,   BrXXX   ,  ACP0   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBReg     , IRT , IXX , IRT , UIMM),
      MTC0       -> List(SType   ,  PC4     ,  toAMU.U,   BrXXX   ,  AReg   ,  BXXX   ,  DCP0   , aluAdd.U   , MemXXX  ,  WBReg     , IXX , IRT , IRD , UIMM)
  )) 
 
//  io.mops.inst_type     := control_signal(0)
  io.mops.next_pc       := control_signal(1)
  io.mops.alu_mdu_lsu   := control_signal(2)
  io.mops.branch_type   := control_signal(3)
  io.mops.src_a         := control_signal(4)
  io.mops.src_b         := control_signal(5)
  io.mops.write_dest    := control_signal(6)
  io.mops.alu_op        := control_signal(7)
  io.mops.mem_width     := control_signal(8)
  io.mops.write_src     := control_signal(9)
  io.mops.rs1           := control_signal(10)
  io.mops.rs2           := control_signal(11)
  io.mops.rd            := control_signal(12)
  io.mops.imm           := control_signal(13)
  io.mops.pc            := io.pc
}
