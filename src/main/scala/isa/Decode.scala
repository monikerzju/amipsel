package isa

import chisel3._
import chisel3.util._
import conf.Config
import ISA._
import MicroOpCtrl._
import fu.{TLBExceptIO, TLBExceptType, TLBOpType}
import icore.InstType

class Mops extends Bundle with Config with InstType with TLBOpType {
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
  val predict_taken = Bool()
  val target_pc     = UInt(len.W)
  val atomic        = if (withBigCore) Bool() else null
  val tlb_exp       = if (withBigCore) new TLBExceptIO else null
  val tlb_op        = if (withBigCore) UInt(TLBOPTYPE_SIZE.W) else null
  val sel           = if (withBigCore) UInt(3.W) else null
}

class DecIO extends Bundle with Config {
  val pc                = Input(UInt(len.W))
  val inst              = Input(UInt(len.W))
  val bht_predict_taken = Input(Bool())
  val target_pc         = Input(UInt(len.W))
  val tlb_exp           = if (withBigCore) Input(new TLBExceptIO) else null
  val mops              = Output(new Mops)
}

class Dec extends Module with InstType with TLBOpType with Config {
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

  val isTlbExp = if (withBigCore) io.tlb_exp.expType =/= TLBExceptType.noExp else false.B

  /**
    * Control signals of all instructions
    */
  // First stage decode, illegal and which fu
  val control_signal_base = Array(
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
    MUL        -> List(F ,  toMDU.U),
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
  )
  val control_signal_ext = Array(
    TLBP       -> List(F ,  toBJU.U),
    TLBR       -> List(F ,  toBJU.U),
    TLBWI      -> List(F ,  toBJU.U),
    CLZ        -> List(F ,  toALU.U),
    SYNC       -> List(F ,  toALU.U),
    WAIT       -> List(F ,  toALU.U),
    PREF       -> List(F ,  toALU.U),
    MADD       -> List(F ,  toMDU.U),
    MADDU      -> List(F ,  toMDU.U),
    TNE        -> List(F ,  toBJU.U),
    MOVZ       -> List(F ,  toBJU.U),
    MOVN       -> List(F ,  toBJU.U),
    LWL        -> List(F ,  toLSU.U),
    LWR        -> List(F ,  toLSU.U),
    SWL        -> List(F ,  toLSU.U),
    SWR        -> List(F ,  toLSU.U),
    LL         -> List(F ,  toLSU.U),       
    SC         -> List(F ,  toLSU.U)
  )
  val control_signal_final = if (withBigCore) Array.concat(control_signal_base, control_signal_ext) else control_signal_base
  val control_signal = ListLookup(io.inst, List(T ,  toBJU.U), control_signal_final)

  val alu_signal_base = Array (
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
  val alu_signal_ext = Array(
    CLZ   -> List(AReg   ,  BXXX , aluClz.U   , IRS , IXX , IRD , SIMM)
  )
  val alu_signal_final = if (withBigCore) Array.concat(alu_signal_base, alu_signal_ext) else alu_signal_base 
  val alu_signal = ListLookup(io.inst, 
    //              src_a  | src_b | aluop      | rs1 | rs2 | rd  |  uimm
               List(AReg   ,  BReg , aluAddu.U  , IXX , IXX , IXX , SIMM), // PREF or WAIT or SYNC
    alu_signal_final
  )

  val mdu_signal_base = Array (
    DIVU  -> List(DHiLo   , MDU_DIVU.U , IRT ),
    MULT  -> List(DHiLo   , MDU_MUL.U  , IRT ),
    MULTU -> List(DHiLo   , MDU_MULU.U , IRT ),
    MTHI  -> List(DHi     , aluAddu.U  , IXX ),
    MTLO  -> List(DLo     , aluAddu.U  , IXX ),
    MUL   -> List(DReg    , MDU_MUL.U  , IRT )
  )
  val mdu_signal_ext = Array (
    MADD  -> List(DHiLoAdd, MDU_MUL.U  , IRT ),
    MADDU -> List(DHiLoAdd, MDU_MULU.U , IRT )
  )
  val mdu_signal_final = if (withBigCore) Array.concat(mdu_signal_base, mdu_signal_ext) else mdu_signal_base
  val mdu_signal = ListLookup(io.inst, 
    //              dest   | mduop      | rs2 
    /*DIV*/  List(DHiLo   , MDU_DIV.U  , IRT ),
    mdu_signal_final
  )

  val lsu_signal_base = Array (
    LBU   -> List(DReg   , MemByteU, IXX , IRT ),
    LH    -> List(DReg   , MemHalf , IXX , IRT ),
    LHU   -> List(DReg   , MemHalfU, IXX , IRT ),
    LW    -> List(DReg   , MemWord , IXX , IRT ),
    SB    -> List(DMem   , MemByte , IRT , IXX ),
    SH    -> List(DMem   , MemHalf , IRT , IXX ),
    SW    -> List(DMem   , MemWord , IRT , IXX )
  )
  val lsu_signal_ext = Array(
    LWL   -> List(DReg   , MemWordL , IRT , IRT ),
    LWR   -> List(DReg   , MemWordR , IRT , IRT ),
    SWL   -> List(DMem   , MemWordL , IRT , IXX ),
    SWR   -> List(DMem   , MemWordR , IRT , IXX ), 
    LL    -> List(DReg   , MemWord  , IXX , IRT ),       
    SC    -> List(DMem   , MemWord  , IRT , IRT )
  )
  val lsu_signal_final = if (withBigCore) Array.concat(lsu_signal_base, lsu_signal_ext) else lsu_signal_base
  val lsu_signal = ListLookup(io.inst, 
    //              dest   | mem     | rs2 | rd 
    /*LB*/     List(DReg   , MemByte , IXX , IRT ),
    lsu_signal_final
  )

  val bju_signal_base = Array (
    BEQ   -> List(Branch  ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IRT , IXX),
    BNE   -> List(Branch  ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IRT , IXX),
    BGEZ  -> List(Branch  ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IXX , IXX),
    BGTZ  -> List(Branch  ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IXX , IXX),
    BLEZ  -> List(Branch  ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IXX , IXX),
    BLTZ  -> List(Branch  ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IXX , IXX),
    BGEZAL-> List(Branch  ,  AReg   ,  DReg      ,  WBALU     , IRS , IXX , IRA),
    BLTZAL-> List(Branch  ,  AReg   ,  DReg      ,  WBALU     , IRS , IXX , IRA),
    J     -> List(Jump    ,  AXXX   ,  DXXX      ,  WBXXX     , IXX , IXX , IXX),
    JAL   -> List(Jump    ,  AXXX   ,  DReg      ,  WBPC      , IXX , IXX , IRA),
    JR    -> List(PCReg   ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IXX , IXX),
    JALR  -> List(PCReg   ,  AReg   ,  DReg      ,  WBPC      , IRS , IXX , IRD),
    BREAK -> List(Break   ,  AXXX   ,  DXXX      ,  WBXXX     , IXX , IXX , IXX),
    SYS   -> List(Trap    ,  AXXX   ,  DXXX      ,  WBXXX     , IXX , IXX , IXX),
    ERET  -> List(Ret     ,  AXXX   ,  DXXX      ,  WBXXX     , IXX , IXX , IXX),
    MFC0  -> List(PC4     ,  ACP0   ,  DReg      ,  WBReg     , IRD , IXX , IRT),
    MTC0  -> List(PC4     ,  AReg   ,  DCP0      ,  WBReg     , IRT , IXX , IRD)
  )
  val bju_signal_ext = Array(
    TNE   -> List(NETrap  ,  AReg   ,  DXXX      ,  WBXXX     , IRS , IRT , IXX),
    MOVN  -> List(PC4     ,  AReg   ,  DRegCond  ,  WBALU     , IRS , IRT , IRD),
    MOVZ  -> List(PC4     ,  AReg   ,  DRegCond  ,  WBALU     , IRS , IRT , IRD)
  )
  val bju_signal_final = if (withBigCore) Array.concat(bju_signal_base, bju_signal_ext) else bju_signal_base
  val bju_signal = ListLookup(io.inst,
    /*Illegal*/List(PC4     ,  AXXX   ,  DXXX  ,  WBXXX     , IXX , IXX , IXX),
    bju_signal_final
  )

  val branch_signal = ListLookup(io.inst, List(BrGE),
    Array (
      BEQ   -> List(BrEQ),
      BNE   -> List(BrNE),
      BGTZ  -> List(BrGT),
      BLEZ  -> List(BrLE),
      BLTZ  -> List(BrLT),
      BLTZAL-> List(BrLT)
    )
  )
  val mov_cond_signal = ListLookup(io.inst, List(BrEQ), // MOVZ
    Array(
      MOVN  -> List(BrNE)
    )
  )

  //     illegal | npc  | fu   | br  |  srca  |  scrb  |  dest  |  aluop  |  memtype  |  src   |   rs  |  rt   | rd   |  imm
  // ALU   0     | pc4  | ?    | xxx |   ?    |  ?     |  dreg  |   ?     |    xxx    |  alu   |    ?  |   ?   |  ?   |   ?      8 fields
  // MDU   0     | pc4  | ?    | xxx | areg   | breg   |  ?     |   ?     |    xxx    |  mdu   |  rs?  |   ?   | xxx  |  simm    4 fields
  // BJU   ?     |  ?   | bj   | ?   |   ?    | bxxx   |   ?    |  alusubu|    xxx    |  ?     |  rs?  | rt?   |  ?   |   ?      10 fields
  // LSU   0     | pc4  | ?    | xxx | areg   | bimm   |   ?    |  add    |   ?       |  xxx   |  rs?  | ?     |  ?   |  simm    6 fields

  if (withBigCore) {
    io.mops.illegal       := control_signal(0).asBool || io.pc(1, 0).orR || isTlbExp
    io.mops.alu_mdu_lsu   := Mux(isTlbExp, toBJU.U, control_signal(1))
    io.mops.branch_type := Mux(control_signal(1) === toBJU.U, Mux(bju_signal(0) === Branch, 
      branch_signal(0), Mux(bju_signal(2) === DRegCond, mov_cond_signal(0), BrXXX)), BrXXX)
  } else {
    io.mops.illegal       := control_signal(0).asBool || io.pc(1, 0).orR
    io.mops.alu_mdu_lsu   := control_signal(1)
    io.mops.branch_type := Mux(control_signal(1) === toBJU.U && bju_signal(0) === Branch, branch_signal(0), BrXXX)
  }
  io.mops.next_pc       := Mux(control_signal(1) === toBJU.U, bju_signal(0), PC4)
  io.mops.src_a         := MuxLookup(control_signal(1), AReg,
                             Seq(
                               toALU.U -> alu_signal(0),
                               toBJU.U -> bju_signal(1)
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
                               toBJU.U -> bju_signal(2)
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
                               toBJU.U -> bju_signal(3)
                             )
                           )
  io.mops.rs1           := MuxLookup(control_signal(1), IRS,
                             Seq(
                               toALU.U -> alu_signal(3),
                               toBJU.U -> bju_signal(4)
                             )
                           )
  io.mops.rs2           := MuxLookup(control_signal(1), lsu_signal(2),
                             Seq(
                               toMDU.U -> mdu_signal(2),
                               toALU.U -> alu_signal(4),
                               toBJU.U -> bju_signal(5)
                             )
                           )
  io.mops.rd            := MuxLookup(control_signal(1), lsu_signal(3),
                             Seq(
                               toMDU.U -> Mux(mdu_signal(0) === DReg, IRD, IXX),
                               toALU.U -> alu_signal(5),
                               toBJU.U -> bju_signal(6)
                             )
                           )
  io.mops.imm           := MuxLookup(control_signal(1), SIMM,
                             Seq(
                               toALU.U -> alu_signal(6),
                               toBJU.U -> Mux(bju_signal(0) === Jump, JOffset, SIMM)
                             )
                           )
  io.mops.pc            := io.pc
  io.mops.predict_taken := io.bht_predict_taken
  io.mops.target_pc     := io.target_pc
  if(withBigCore){
    io.mops.atomic      := io.inst === SC || io.inst === LL
    io.mops.tlb_exp     := io.tlb_exp
    io.mops.tlb_op      := ListLookup(io.inst, List(notlb.U),
                            Array (
                              TLBP  -> List(tlbp.U),
                              TLBR  -> List(tlbr.U),
                              TLBWI -> List(tlbwi.U),
                              TLBWR -> List(tlbwr.U)
                            )
                          )(0)
    io.mops.sel         := io.inst(2,0)
    when(io.inst === MOVN || io.inst === MOVZ){
      io.mops.src_b := BZero
    }
  }
}
