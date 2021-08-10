package isa 

import fu.MDUOperation
import chisel3._ 
import chisel3.util._ 

object MicroOpCtrl extends MDUOperation {
  val AMN           = "b110".U(3.W) 
  val ANN           = "b100".U(3.W) 
  val NMN           = "b010".U(3.W)
  val NNL           = "b001".U(3.W)

  // Maybe replaced by CFI Type and Illegal
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
  val DMem          = 0.U(3.W)
  val DReg          = 1.U(3.W)
  val DCP0          = 2.U(3.W)
  val DHi           = 3.U(3.W)
  val DLo           = 4.U(3.W)
  val DHiLo         = 5.U(3.W)
  val SZ_W_DEST     = DXXX.getWidth

  // correspond with MemAccessType
  val MemXXX        = 4.U(3.W)
  val MemWord       = 2.U(3.W)  // Word 010
  val MemByte       = 0.U(3.W)  // Byte 000
  val MemByteU      = 6.U(3.W)  // Byte 110
  val MemHalf       = 1.U(3.W)  // Half 001
  val MemHalfU      = 5.U(3.W)  // Half 101
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
  val Ret           = 5.U(3.W)
  val Break         = 6.U(3.W)
  val NETrap        = 7.U(3.W)
  val SZ_NEXT_PC    = PC4.getWidth

  val SZ_ISSUE_FU   = 0

  val SZ_ALU_TYPE   = aluOpWidth

  val SZ_REG_ADDR   = 5

  val SZ_MICRO_OP = (new Mops).getWidth
}