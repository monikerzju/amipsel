package isa

import chisel3._ 
import chisel3.util._ 
import conf.Config
import fu.MDUOperation
import ISA._

trait MicroOpCtrl extends MDUOperation {
  val T = true.B 
  val F = false.B 

  val InstXXX       = 0.U(4.W)
  val RType         = 1.U(4.W)
  val RSType        = 2.U(4.W)
  val RTType        = 3.U(4.W)
  val IType         = 4.U(4.W)
  val IBType        = 5.U(4.W)
  val IMType        = 6.U(4.W)
  val JType         = 7.U(4.W)
  val JRType        = 8.U(4.W)
  val SType         = 9.U(4.W)
  val Illegal       = 15.U(4.W)
  val SZ_INST_TYPE  = InstXXX.getWidth

  val BrXXX         = 0.U(4.W)
  val BrEQ          = 1.U(4.W)
  val BrNE          = 2.U(4.W)
  val BrGE          = 3.U(4.W)
  val BrGT          = 4.U(4.W)
  val BrLE          = 5.U(4.W)
  val BrLT          = 6.U(4.W)
  val Except        = 9.U(4.W)
  val SZ_BR_TYPE    = BrXXX.getWidth

  val AXXX          = 0.U(3.W)
  val AReg          = 0.U(3.W)
  val ACP0          = 1.U(3.W)
  val AHi           = 2.U(3.W)
  val ALo           = 3.U(3.W)
  val AShamt        = 4.U(3.W)
  val SZ_ALU_A      = AXXX.getWidth

  val BXXX          = 0.U(1.W)
  val BReg          = 0.U(1.W)
  val BImm          = 1.U(1.W)
  val SZ_ALU_B      = BXXX.getWidth

  val DXXX          = 0.U(3.W)
  val DMem          = 1.U(3.W)
  val DReg          = 2.U(3.W)
  val DCP0          = 3.U(3.W)
  val DHi           = 4.U(3.W)
  val DLo           = 5.U(3.W)
  val DHiLo         = 6.U(3.W)
  val SZ_W_DEST     = DXXX.getWidth

  val MemXXX        = 0.U(3.W)
  val MemWord       = 1.U(3.W)
  val MemByte       = 2.U(3.W)
  val MemByteU      = 3.U(3.W)
  val MemHalf       = 4.U(3.W)
  val MemHalfU      = 5.U(3.W)
  val SZ_MEM_TYPE   = MemXXX.getWidth

  val WBXXX         = 0.U(2.W)
  val WBReg         = 0.U(2.W)
  val WBALU         = 1.U(2.W)
  val WBMEM         = 2.U(2.W)
  val WBPC          = 3.U(2.W)
  val SZ_WB_SRC     = WBXXX.getWidth

  val PC4           = 0.U(3.W)
  val PCReg         = 1.U(3.W)
  val Branch        = 2.U(3.W)
  val Jump          = 3.U(3.W)
  val Trap          = 4.U(3.W)
  val SZ_NEXT_PC    = PC4.getWidth

  val SZ_ISSUE_FU   = 0

  val SZ_ALU_TYPE   = aluOpWidth

  val SZ_REG_ADDR   = 5

  val SZ_MICRO_OP = (new Mops).getWidth
}

class Mops extends Bundle with MicroOpCtrl {
  val inst_type     = UInt(SZ_INST_TYPE.W)
  val next_pc       = UInt(SZ_NEXT_PC.W)
  val mdu           = Bool()
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
}

class DecIO extends Bundle with Config with MicroOpCtrl {
  val inst = Input(UInt(len.W))
  val mops = Output(new Mops)
}

class Dec extends Module with MicroOpCtrl {
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
                    List(Illegal ,  PC4     ,  F   ,  BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd  , MemXXX  ,  WBXXX     , IRS , IRT , IRD),
    Array(         /*      Inst  |   PC     | use      | Branch   |   A     |   B     |  D      | alu     |  Mem    |     wb     | rs1 | rs2 |  rd */
                   /*      Type  | Select   | mult     | Type     | use rs1 | use rs2 | write   | Type    | Type    |   Select   |     |     |     */
                   /*  Structure | NextPC   | Mult/Div | Brch/Jmp | alusrcA | alusrcB | target  | alu OP  | B/H/W   | MultiIssue |     |     |     */
      NOP        -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      ADD        -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAdd   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ADDI       -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      ADDU       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAddu  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ADDIU      -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAddu  , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      SUB        -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSub   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SUBU       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSub   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLT        -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSlt   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLTI       -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluSlt   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      SLTU       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSltu  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLTIU      -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluSltu  , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      DIV        -> List(RType   ,  PC4     ,  T   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_DIV  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      DIVU       -> List(RType   ,  PC4     ,  T   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_DIVU , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      MULT       -> List(RType   ,  PC4     ,  T   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_MUL  , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      MULTU      -> List(RType   ,  PC4     ,  T   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , MDU_MULU , MemXXX  ,  WBALU     , IRS , IRT , IRD),
           
      AND        -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluAnd   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ANDI       -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAnd   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      LUI        -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluLui   , MemXXX  ,  WBALU     , IXX , IXX , IRT),
      NOR        -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluNor   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      OR         -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluOr    , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      ORI        -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluOr    , MemXXX  ,  WBALU     , IRS , IXX , IRT),
      XOR        -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluXor   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      XORI       -> List(IType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluXor   , MemXXX  ,  WBALU     , IRS , IXX , IRT),
           
      SLLV       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSll   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SLL        -> List(RSType  ,  PC4     ,  F   ,      BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSll   , MemXXX  ,  WBALU     , IXX , IRT , IRD),
      SRAV       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSra   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SRA        -> List(RSType  ,  PC4     ,  F   ,      BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSra   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
      SRLV       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BReg   ,  DReg   , aluSrl   , MemXXX  ,  WBALU     , IRS , IRT , IRD),
      SRL        -> List(RSType  ,  PC4     ,  F   ,      BrXXX   ,  AShamt ,  BReg   ,  DReg   , aluSrl   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
           
      BEQ        -> List(IBType  ,  Branch  ,  F   ,      BrEQ    ,  AReg   ,  BReg   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BNE        -> List(IBType  ,  Branch  ,  F   ,      BrNE    ,  AReg   ,  BReg   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BGEZ       -> List(IBType  ,  Branch  ,  F   ,      BrGE    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BGTZ       -> List(IBType  ,  Branch  ,  F   ,      BrGT    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BLEZ       -> List(IBType  ,  Branch  ,  F   ,      BrLE    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BLTZ       -> List(IBType  ,  Branch  ,  F   ,      BrLT    ,  AReg   ,  BXXX   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IRT , IXX),
      BGEZAL     -> List(IBType  ,  Branch  ,  F   ,      BrGE    ,  AReg   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBALU     , IRS , IRT , IRA),
      BLTZAL     -> List(IBType  ,  Branch  ,  F   ,      BrLT    ,  AReg   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBALU     , IRS , IRT , IRA),
           
      J          -> List(JType   ,  Jump    ,  F   ,      BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
      JAL        -> List(JType   ,  Jump    ,  F   ,      BrXXX   ,  AXXX   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBPC      , IXX , IXX , IRA),
      JR         -> List(JRType  ,  PCReg   ,  F   ,      BrXXX   ,  AReg   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBXXX     , IRS , IXX , IRD),
      JALR       -> List(JRType  ,  PCReg   ,  F   ,      BrXXX   ,  AReg   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBPC      , IRS , IXX , IRA),
           
      MFHI       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AHi    ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
      MFLO       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  ALo    ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBALU     , IXX , IXX , IRD),
      MTHI       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BXXX   ,  DHi    , aluAdd   , MemXXX  ,  WBALU     , IRS , IXX , IXX),
      MTLO       -> List(RType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BXXX   ,  DLo    , aluAdd   , MemXXX  ,  WBALU     , IRS , IXX , IXX),
           
      BREAK      -> List(RTType  ,  PC4     ,  F   ,      Except  ,  AXXX   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
      SYSCALL    -> List(RTType  ,  PC4     ,  F   ,      Except  ,  AXXX   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
           
      LB         -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd   , MemByte ,  WBMEM     , IRS , IXX , IRT),
      LBU        -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd   , MemByteU,  WBMEM     , IRS , IXX , IRT),
      LH         -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd   , MemHalf ,  WBMEM     , IRS , IXX , IRT),
      LHU        -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd   , MemHalfU,  WBMEM     , IRS , IXX , IRT),
      LW         -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DReg   , aluAdd   , MemWord ,  WBMEM     , IRS , IXX , IRT),
      SB         -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd   , MemByte ,  WBXXX     , IRS , IRT , IXX),
      SH         -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd   , MemHalf ,  WBXXX     , IRS , IRT , IXX),
      SW         -> List(IMType  ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BImm   ,  DMem   , aluAdd   , MemWord ,  WBXXX     , IRS , IRT , IXX),
           
      ERET       -> List(SType   ,  PC4     ,  F   ,      BrXXX   ,  AXXX   ,  BXXX   ,  DXXX   , aluAdd   , MemXXX  ,  WBXXX     , IXX , IXX , IXX),
      MFC0       -> List(SType   ,  PC4     ,  F   ,      BrXXX   ,  ACP0   ,  BXXX   ,  DReg   , aluAdd   , MemXXX  ,  WBReg     , IRT , IXX , IRT),
      MTC0       -> List(SType   ,  PC4     ,  F   ,      BrXXX   ,  AReg   ,  BXXX   ,  DCP0   , aluAdd   , MemXXX  ,  WBReg     , IXX , IRT , IRD)
  )) 
 
  io.mops.inst_type     := control_signal(0)
  io.mops.next_pc       := control_signal(1)
  io.mops.mdu           := control_signal(2)
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
}
