package isa

import chisel3._ 
import chisel3.util._ 
import conf.Config
import ISA._
import MicroOpCtrl._

class Mops extends Bundle with Config {
//  val inst_type     = UInt(SZ_INST_TYPE.W)
  val next_pc       = UInt(SZ_NEXT_PC.W)
  val alu_mdu_lsu   = UInt(3.W)
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
  val imm           = UInt(16.W)
  val pc            = UInt(len.W)
}

class DecIO extends Bundle with Config {
  val pc   = Input(UInt(len.W))
  val inst = Input(UInt(len.W))
  val mops = Output(new Mops)
}

class Dec extends Module {
  val io = IO(new DecIO)

  val SHAMT = io.inst(10,  6)
  val IMM   = io.inst(15,  0)
  val IRS   = io.inst(25, 21)
  val IRT   = io.inst(20, 16)
  val IRD   = io.inst(15, 11)
  val IRA   = 31.U(SZ_REG_ADDR.W)
  val IXX   = 0.U(SZ_REG_ADDR.W)

  // Decode
  val control_signal = ListLookup(io.inst,
                    List(Illegal ,  PC4     ,  ANN ,      BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IRD),
    Array(         /*      Inst  |   PC     | use      | Branch   |   A     |   B     |  D      | alu        |  Mem    |     wb     | rs1 | rs2 |  rd */
                   /*      Type  | Select   | which    | Type     | use rs1 | use rs2 | write   | Type       | Type    |   Select   |     |     |     */
                   /*  Structure | NextPC   | al md ls | Brch/Jmp | alusrcA | alusrcB | target  | alu OP     | B/H/W   | MultiIssue |     |     |     */
      NOP        -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      ADD        -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ADDI       -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      ADDU       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAddu.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ADDIU      -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAddu.U  , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      SUB        -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSub.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SUBU       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSub.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLT        -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSlt.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLTI       -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluSlt.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      SLTU       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSltu.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLTIU      -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluSltu.U  , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      DIV        -> List(RType   ,  PC4     ,  NMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_DIV.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      DIVU       -> List(RType   ,  PC4     ,  NMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_DIVU.U , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      MULT       -> List(RType   ,  PC4     ,  NMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_MUL.U  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      MULTU      -> List(RType   ,  PC4     ,  NMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_MULU.U , MemXXX  ,  WBALU     , IRS , IRT , IRD),
           
      AND        -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAnd.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ANDI       -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAnd.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      LUI        -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluLui.U   , MemXXX  ,  WBALU     , IXX , IXX , IRT),
      NOR        -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluNor.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      OR         -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluOr.U    , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ORI        -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluOr.U    , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      XOR        -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluXor.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      XORI       -> List(IType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluXor.U   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
           
      SLLV       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSll.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLL        -> List(RSType  ,  PC4     ,  AMN ,      BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSll.U   , MemXXX  ,  WBALU     , IXX , IRT , IRD),
      SRAV       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSra.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SRA        -> List(RSType  ,  PC4     ,  AMN ,      BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSra.U   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
      SRLV       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSrl.U   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SRL        -> List(RSType  ,  PC4     ,  AMN ,      BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSrl.U   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
           
      BEQ        -> List(IBType  ,  Branch  ,  ANN ,      BrEQ    ,  AReg   ,  BReg   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BNE        -> List(IBType  ,  Branch  ,  ANN ,      BrNE    ,  AReg   ,  BReg   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BGEZ       -> List(IBType  ,  Branch  ,  ANN ,      BrGE    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BGTZ       -> List(IBType  ,  Branch  ,  ANN ,      BrGT    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BLEZ       -> List(IBType  ,  Branch  ,  ANN ,      BrLE    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BLTZ       -> List(IBType  ,  Branch  ,  ANN ,      BrLT    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BGEZAL     -> List(IBType  ,  Branch  ,  ANN ,      BrGE    ,  AReg   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IRT , IRA),
      BLTZAL     -> List(IBType  ,  Branch  ,  ANN ,      BrLT    ,  AReg   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IRT , IRA),
           
      J          -> List(JType   ,  Jump    ,  ANN ,      BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
      JAL        -> List(JType   ,  Jump    ,  ANN ,      BrXXX   ,  AXXX   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBPC      , IXX , IXX , IRA),
      JR         -> List(JRType  ,  PCReg   ,  ANN ,      BrXXX   ,  AReg   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBXXX     , IRS , IXX , IRD),
      JALR       -> List(JRType  ,  PCReg   ,  ANN ,      BrXXX   ,  AReg   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBPC      , IRS , IXX , IRA),
           
      MFHI       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AHi    ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
      MFLO       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  ALo    ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
      MTHI       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BXXX   ,  DHi    , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IXX , IXX),
      MTLO       -> List(RType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BXXX   ,  DLo    , aluAdd.U   , MemXXX  ,  WBALU     , IRS , IXX , IXX),
           
      BREAK      -> List(RTType  ,  Trap    ,  AMN ,      BrXXX   ,  AXXX   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
      SYSCALL    -> List(RTType  ,  Trap    ,  AMN ,      BrXXX   ,  AXXX   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
           
      LB         -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemByte ,  WBMEM     , IRS , IXX , IRT),
      LBU        -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemByteU,  WBMEM     , IRS , IXX , IRT),
      LH         -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemHalf ,  WBMEM     , IRS , IXX , IRT),
      LHU        -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemHalfU,  WBMEM     , IRS , IXX , IRT),
      LW         -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd.U   , MemWord ,  WBMEM     , IRS , IXX , IRT),
      SB         -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd.U   , MemByte ,  WBXXX     , IRS , IRT , IXX),
      SH         -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd.U   , MemHalf ,  WBXXX     , IRS , IRT , IXX),
      SW         -> List(IMType  ,  PC4     ,  NNL ,      BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd.U   , MemWord ,  WBXXX     , IRS , IRT , IXX),
           
      ERET       -> List(SType   ,  Ret     ,  AMN ,      BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd.U   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
      MFC0       -> List(SType   ,  PC4     ,  AMN ,      BrXXX   ,  ACP0   ,  BXXX   ,  DReg   , aluAdd.U   , MemXXX  ,  WBReg     , IRT , IXX , IRT),
      MTC0       -> List(SType   ,  PC4     ,  AMN ,      BrXXX   ,  AReg   ,  BXXX   ,  DCP0   , aluAdd.U   , MemXXX  ,  WBReg     , IXX , IRT , IRD)
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
  io.mops.imm           := IMM
  io.mops.pc            := io.pc
}
