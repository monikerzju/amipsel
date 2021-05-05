package isa

import chisel3._ 
import chisel3.util._ 
import conf.Config
import ISA._
import MicroOpCtrl._
import icore.InstType

class Mops extends Bundle with Config with InstType {
  val illegal       = Bool()
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

  def T = true.B
  def F = false.B

  // First stage decode, illegal and which fu
  val control_signal = ListLookup(io.inst,
                    List(T ,  toBJU.U),
    Array(
      NOP        -> List(F ,  toALU.U),
      ADD        -> List(F ,  toALU.U),
      ADDI       -> List(F ,  toALU.U),
      ADDU       -> List(F ,  toALU.U),
      ADDIU      -> List(F ,  toALU.U),
      SUB        -> List(F ,  toALU.U),
      SUBU       -> List(F ,  toALU.U),
      SLT        -> List(F ,  toALU.U),
      SLTI       -> List(F ,  toALU.U),
      SLTU       -> List(F ,  toALU.U),
      SLTIU      -> List(F ,  toALU.U),
      DIV        -> List(F ,  toMDU.U),
      DIVU       -> List(F ,  toMDU.U),
      MULT       -> List(F ,  toMDU.U),
      MULTU      -> List(F ,  toMDU.U),      
      AND        -> List(F ,  toALU.U),
      ANDI       -> List(F ,  toALU.U),
      LUI        -> List(F ,  toALU.U),
      NOR        -> List(F ,  toALU.U),
      OR         -> List(F ,  toALU.U),
      ORI        -> List(F ,  toALU.U),
      XOR        -> List(F ,  toALU.U),
      XORI       -> List(F ,  toALU.U),      
      SLLV       -> List(F ,  toALU.U),
      SLL        -> List(F ,  toALU.U),
      SRAV       -> List(F ,  toALU.U),
      SRA        -> List(F ,  toALU.U),
      SRLV       -> List(F ,  toALU.U),
      SRL        -> List(F ,  toALU.U),     
      BEQ        -> List(F ,  toBJU.U),
      BNE        -> List(F ,  toBJU.U),
      BGEZ       -> List(F ,  toBJU.U),
      BGTZ       -> List(F ,  toBJU.U),
      BLEZ       -> List(F ,  toBJU.U),
      BLTZ       -> List(F ,  toBJU.U),
      BGEZAL     -> List(F ,  toBJU.U),
      BLTZAL     -> List(F ,  toBJU.U),    
      J          -> List(F ,  toBJU.U),
      JAL        -> List(F ,  toBJU.U),
      JR         -> List(F ,  toBJU.U),
      JALR       -> List(F ,  toBJU.U),     
      MFHI       -> List(F ,  toALU.U),
      MFLO       -> List(F ,  toALU.U),
      MTHI       -> List(F ,  toMDU.U),
      MTLO       -> List(F ,  toMDU.U),     
      BREAK      -> List(F ,  toBJU.U),
      SYS        -> List(F ,  toBJU.U),     
      LB         -> List(F ,  toLSU.U),
      LBU        -> List(F ,  toLSU.U),
      LH         -> List(F ,  toLSU.U),
      LHU        -> List(F ,  toLSU.U),
      LW         -> List(F ,  toLSU.U),
      SB         -> List(F ,  toLSU.U),
      SH         -> List(F ,  toLSU.U),
      SW         -> List(F ,  toLSU.U),     
      ERET       -> List(F ,  toBJU.U),
      MFC0       -> List(F ,  toBJU.U),
      MTC0       -> List(F ,  toBJU.U)
  )) 

  val alu_signal = ListLookup(io.inst, 
    //              src_a  | src_b | aluop      | rs1 | rs2 | rd  |  uimm
    /*NOP*/    List(AReg   ,  BReg , aluAddu.U  , IXX , IXX , IXX , SIMM),
    Array (
      ADD   -> List(AReg   ,  BReg , aluAdd.U   , IRS , IRT , IRD , SIMM),
      ADDI  -> List(AReg   ,  BImm , aluAdd.U   , IRS , IXX , IRT , SIMM),
      ADDU  -> List(AReg   ,  BReg , aluAddu.U  , IRS , IRT , IRD , SIMM),
      ADDIU -> List(AReg   ,  BImm , aluAddu.U  , IRS , IXX , IRT , SIMM),
      SUB   -> List(AReg   ,  BReg , aluSub.U   , IRS , IRT , IRD , SIMM),
      SUBU  -> List(AReg   ,  BReg , aluSubu.U  , IRS , IRT , IRD , SIMM),
      SLT   -> List(AReg   ,  BReg , aluSlt.U   , IRS , IRT , IRD , SIMM),
      SLTI  -> List(AReg   ,  BImm , aluSlt.U   , IRS , IXX , IRT , SIMM),
      SLTU  -> List(AReg   ,  BReg , aluSltu.U  , IRS , IRT , IRD , SIMM),
      SLTIU -> List(AReg   ,  BImm , aluSltu.U  , IRS , IXX , IRT , SIMM),
      AND   -> List(AReg   ,  BReg , aluAnd.U   , IRS , IRT , IRD , SIMM),
      ANDI  -> List(AReg   ,  BImm , aluAnd.U   , IRS , IXX , IRT , UIMM),
      LUI   -> List(AReg   ,  BImm , aluLui.U   , IXX , IXX , IRT , SIMM),
      NOR   -> List(AReg   ,  BReg , aluNor.U   , IRS , IRT , IRD , SIMM),
      OR    -> List(AReg   ,  BReg , aluOr.U    , IRS , IRT , IRD , SIMM),
      ORI   -> List(AReg   ,  BImm , aluOr.U    , IRS , IXX , IRT , UIMM),
      XOR   -> List(AReg   ,  BReg , aluXor.U   , IRS , IRT , IRD , SIMM),
      XORI  -> List(AReg   ,  BImm , aluXor.U   , IRS , IXX , IRT , UIMM),
      SLLV  -> List(AReg   ,  BReg , aluSll.U   , IRS , IRT , IRD , SIMM),
      SLL   -> List(AShamt ,  BReg , aluSll.U   , IXX , IRT , IRD , SIMM),
      SRAV  -> List(AReg   ,  BReg , aluSra.U   , IRS , IRT , IRD , SIMM),
      SRA   -> List(AShamt ,  BReg , aluSra.U   , IXX , IRT , IRD , SIMM),
      SRLV  -> List(AReg   ,  BReg , aluSrl.U   , IRS , IRT , IRD , SIMM),
      SRL   -> List(AShamt ,  BReg , aluSrl.U   , IXX , IRT , IRD , SIMM),
      MFHI  -> List(AHi    ,  BXXX , aluAddu.U  , IXX , IXX , IRD , SIMM),
      MFLO  -> List(ALo    ,  BXXX , aluAddu.U  , IXX , IXX , IRD , SIMM)
    )
  )

  val mdu_signal = ListLookup(io.inst, 
    //              dest   | mduop      | rs2 
    /*DIV*/    List(DHiLo  , MDU_DIV.U  , IRT ),
    Array (
      DIVU  -> List(DHiLo  , MDU_DIVU.U , IRT ),
      MULT  -> List(DHiLo  , MDU_MUL.U  , IRT ),
      MULTU -> List(DHiLo  , MDU_MULU.U , IRT ),
      MTHI  -> List(DHi    , aluAddu.U  , IXX ),
      MTLO  -> List(DLo    , aluAddu.U  , IXX )
    )
  )

  val lsu_signal = ListLookup(io.inst, 
    //              dest   | mem     | rs2 | rd 
    /*LB*/     List(DReg   , MemByte , IXX , IRT ),
    Array (
      LBU   -> List(DReg   , MemByteU, IXX , IRT ),
      LH    -> List(DReg   , MemHalf , IXX , IRT ),
      LHU   -> List(DReg   , MemHalfU, IXX , IRT ),
      LW    -> List(DReg   , MemWord , IXX , IRT ),
      SB    -> List(DMem   , MemByte , IRT , IXX ),
      SH    -> List(DMem   , MemHalf , IRT , IXX ),
      SW    -> List(DMem   , MemWord , IRT , IXX )
    )
  )

  val bju_signal = ListLookup(io.inst,
    /*Illegal*/List(PC4     , BrXXX   ,  AXXX   ,  DXXX  ,  WBXXX     , IXX , IXX , IXX, SIMM   ),
    Array (
      BEQ   -> List(Branch  , BrEQ    ,  AReg   ,  DXXX  ,  WBXXX     , IRS , IRT , IXX, SIMM   ),
      BNE   -> List(Branch  , BrNE    ,  AReg   ,  DXXX  ,  WBXXX     , IRS , IRT , IXX, SIMM   ),
      BGEZ  -> List(Branch  , BrGE    ,  AReg   ,  DXXX  ,  WBXXX     , IRS , IXX , IXX, SIMM   ),
      BGTZ  -> List(Branch  , BrGT    ,  AReg   ,  DXXX  ,  WBXXX     , IRS , IXX , IXX, SIMM   ),
      BLEZ  -> List(Branch  , BrLE    ,  AReg   ,  DXXX  ,  WBXXX     , IRS , IXX , IXX, SIMM   ),
      BLTZ  -> List(Branch  , BrLT    ,  AReg   ,  DXXX  ,  WBXXX     , IRS , IXX , IXX, SIMM   ),
      BGEZAL-> List(Branch  , BrGE    ,  AReg   ,  DReg  ,  WBALU     , IRS , IXX , IRA, SIMM   ),
      BLTZAL-> List(Branch  , BrLT    ,  AReg   ,  DReg  ,  WBALU     , IRS , IXX , IRA, SIMM   ),
      J     -> List(Jump    , BrXXX   ,  AXXX   ,  DXXX  ,  WBXXX     , IXX , IXX , IXX, JOffset),
      JAL   -> List(Jump    , BrXXX   ,  AXXX   ,  DReg  ,  WBPC      , IXX , IXX , IRA, JOffset),
      JR    -> List(PCReg   , BrXXX   ,  AReg   ,  DXXX  ,  WBXXX     , IRS , IXX , IXX, SIMM   ),
      JALR  -> List(PCReg   , BrXXX   ,  AReg   ,  DReg  ,  WBPC      , IRS , IXX , IRD, SIMM   ),
      BREAK -> List(Break   , BrXXX   ,  AXXX   ,  DXXX  ,  WBXXX     , IXX , IXX , IXX, SIMM   ),
      SYS   -> List(Trap    , BrXXX   ,  AXXX   ,  DXXX  ,  WBXXX     , IXX , IXX , IXX, SIMM   ),
      ERET  -> List(Ret     , BrXXX   ,  AXXX   ,  DXXX  ,  WBXXX     , IXX , IXX , IXX, SIMM   ),
      MFC0  -> List(PC4     , BrXXX   ,  ACP0   ,  DReg  ,  WBReg     , IRD , IXX , IRT, SIMM   ),
      MTC0  -> List(PC4     , BrXXX   ,  AReg   ,  DCP0  ,  WBReg     , IRT , IXX , IRD, SIMM   )
    )    
  )

  //     illegal | npc  | fu   | br  |  srca  |  scrb  |  dest  |  aluop  |  memtype  |  src   |   rs  |  rt   | rd   |  imm
  // ALU   0     | pc4  | ?    | xxx |   ?    |  ?     |  dreg  |   ?     |    xxx    |  alu   |    ?  |   ?   |  ?   |   ?      8 fields
  // MDU   0     | pc4  | ?    | xxx | areg   | breg   |  ?     |   ?     |    xxx    |  mdu   |  rs?  |   ?   | xxx  |  simm    4 fields
  // BJU   ?     |  ?   | bj   | ?   |   ?    | bxxx   |   ?    |  alusubu|    xxx    |  ?     |  rs?  | rt?   |  ?   |   ?      10 fields
  // LSU   0     | pc4  | ?    | xxx | areg   | bimm   |   ?    |  add    |   ?       |  xxx   |  rs?  | ?     |  ?   |  simm    6 fields
 
  io.mops.illegal       := control_signal(0).asBool || io.pc(1, 0).orR
  io.mops.next_pc       := Mux(control_signal(1) === toBJU.U, bju_signal(0), PC4)
  io.mops.alu_mdu_lsu   := control_signal(1)
  io.mops.branch_type   := Mux(control_signal(1) === toBJU.U, bju_signal(1), BrXXX)
  io.mops.src_a         := MuxLookup(control_signal(1), AReg,
                             Seq(
                               toALU.U -> alu_signal(0),
                               toBJU.U -> bju_signal(2)
                             )
                           )
  io.mops.src_b         := MuxLookup(control_signal(1), BReg,
                             Seq(
                               toALU.U -> alu_signal(1),
                               toLSU.U -> BImm
                             )
                           )
  io.mops.write_dest    := MuxLookup(control_signal(1), DReg,
                             Seq(
                               toMDU.U -> mdu_signal(0),
                               toLSU.U -> lsu_signal(0),
                               toBJU.U -> bju_signal(3)
                             )
                           )
  io.mops.alu_op        := MuxLookup(control_signal(1), aluAddu.U,
                             Seq(
                               toMDU.U -> mdu_signal(1),
                               toALU.U -> alu_signal(2),
                               toBJU.U -> aluSubu.U
                             )
                           )
  io.mops.mem_width     := Mux(control_signal(1) === toLSU.U, lsu_signal(1), MemXXX)
  io.mops.write_src     := MuxLookup(control_signal(1), WBALU,
                             Seq(
                               toLSU.U -> WBMEM,
                               toBJU.U -> bju_signal(4)
                             )
                           )
  io.mops.rs1           := MuxLookup(control_signal(1), IRS,
                             Seq(
                               toALU.U -> alu_signal(3),
                               toBJU.U -> bju_signal(5)
                             )
                           )
  io.mops.rs2           := MuxLookup(control_signal(1), lsu_signal(2),
                             Seq(
                               toMDU.U -> mdu_signal(2),
                               toALU.U -> alu_signal(4),
                               toBJU.U -> bju_signal(6)
                             )
                           )
  io.mops.rd            := MuxLookup(control_signal(1), lsu_signal(3),
                             Seq(
                               toMDU.U -> IXX,
                               toALU.U -> alu_signal(5),
                               toBJU.U -> bju_signal(7)
                             )
                           )
  io.mops.imm           := MuxLookup(control_signal(1), SIMM,
                             Seq(
                               toALU.U -> alu_signal(6),
                               toBJU.U -> bju_signal(8)
                             )
                           )
  io.mops.pc            := io.pc
}
