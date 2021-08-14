package fu

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import conf.Config

trait RefType {
  val fetch = 0
  val load = 1
  val store = 2
  val REFTYPE_SIZE = 2
}

trait TLBOpType {
  val notlb = 0
  val tlbp = 1
  val tlbr = 2
  val tlbwi = 3
  val tlbwr = 4
  val TLBOPTYPE_SIZE = 3
}

object TLBExceptType {
  val TLBEXPTYPE_SIZE = 2
  val noExp = 0.U(TLBEXPTYPE_SIZE.W)
  val mod = 1.U(TLBEXPTYPE_SIZE.W)
  val tlbl = 2.U(TLBEXPTYPE_SIZE.W)
  val tlbs = 3.U(TLBEXPTYPE_SIZE.W)

}

class TLBEntryIO extends Bundle with Config {
  val entryHi = Output(new EntryHiStruct)
  val entryLo = Output(Vec(2, new EntryLoStruct))
  val pageMask = Output(new PageMaskStruct)
  val index = Output(new IndexStruct)
}

class TLBExceptIO extends Bundle with Config {
  val expType = UInt(TLBExceptType.TLBEXPTYPE_SIZE.W)
  val expVec = Bool()
  val badVaddr = UInt(len.W)
  val vpn = UInt(VPNSize.W)
  // TODO: asid
}

class TLBAddrTranslIO extends Bundle with Config with RefType {
  val refType = Input(UInt(REFTYPE_SIZE.W))
  val virt_addr = Input(UInt(len.W))
  val phys_addr = Output(UInt(len.W))
  val isFind = Output(Bool())
  val exp = Output(new TLBExceptIO)
}

class TLBOpIO extends Bundle with Config with TLBOpType {
  val op = Input(UInt(TLBOPTYPE_SIZE.W))
  val din = Flipped(new TLBEntryIO)
  val dout = new TLBEntryIO
}

class TLBIO(port: Int = 3) extends Bundle with Config {
  val addrTransl = Vec(port, new TLBAddrTranslIO)
  val execOp = new TLBOpIO
  override def cloneType = new TLBIO(port).asInstanceOf[this.type]
}


class TLBEntry extends Bundle {
  class TLBEntryHi extends Bundle {
    val vpn2 = UInt(19.W)
    val asid = UInt(8.W)
  }
  class TLBEntryLo extends Bundle {
    val pfn = UInt(20.W)
    val c = UInt(3.W)
    val d = UInt(1.W)
    val v = UInt(1.W)
  }
  def getEntryHiClass(): TLBEntryHi = {
    new TLBEntryHi
  }
  def getEntryLoClass(): TLBEntryLo = {
    new TLBEntryLo
  }
  val entryHi = new TLBEntryHi
  val pageMask = UInt(12.W)
  val g = UInt(1.W)
  val entryLo = Vec(2, new TLBEntryLo)
}

class TLB(port: Int = 3) extends Module with Config with RefType with TLBOpType {
  val io = IO(new TLBIO(port))
  // TODO: check TLB initialize
  val entries = Reg(Vec(TLBSize ,new TLBEntry))
  val n = log2Up(TLBSize)

  // tlb operation
  io.execOp.dout := 0.U((new TLBEntryIO).getWidth.W).asTypeOf(new TLBEntryIO)
  switch (io.execOp.op) {
    is (tlbp.U) {

      val findIndex = Wire(Bool())
      val idx = Wire(UInt(n.W))
      findIndex := false.B
      idx := 0.U
      for(i <- 0 until TLBSize) {
        when ((entries(i).entryHi.vpn2 === io.execOp.din.entryHi.vpn2)
          && (entries(i).g.asBool() || entries(i).entryHi.asid === io.execOp.din.entryHi.asid)) {
          idx := i.U
          findIndex := true.B
        }
      }
      when (findIndex) {
        io.execOp.dout.index := Cat(0.U((len - n).W), idx).asTypeOf(new IndexStruct)
      } .otherwise { // not find entry
        io.execOp.dout.index := Cat(1.U(1.W), 0.U((len - 1).W)).asTypeOf(new IndexStruct)
      }

    }
    is (tlbr.U) {
      when (io.execOp.din.index.index < TLBSize.U) {
        val entry = entries(io.execOp.din.index.index)
        io.execOp.dout.pageMask := Cat(0.U(7.W), entry.pageMask, 0.U(13.W)).asTypeOf(new PageMaskStruct)
        io.execOp.dout.entryHi := Cat(entry.entryHi.vpn2, 0.U(5.W), entry.entryHi.asid).asTypeOf(new EntryHiStruct)
        io.execOp.dout.entryLo(0) := Cat(0.U(6.W), entry.entryLo(0).asUInt(), entry.g).asTypeOf(new EntryLoStruct)
        io.execOp.dout.entryLo(1) := Cat(0.U(6.W), entry.entryLo(1).asUInt(), entry.g).asTypeOf(new EntryLoStruct)
      }
    }
    is (tlbwi.U) {
      when (io.execOp.din.index.index < TLBSize.U) {
        val entry = entries(io.execOp.din.index.index)
        entry.pageMask := io.execOp.din.pageMask.mask
        entry.entryHi := Cat(io.execOp.din.entryHi.vpn2, io.execOp.din.entryHi.asid).asTypeOf((new TLBEntry).getEntryHiClass())
        for (i <- 0 until 2) {
          entry.entryLo(i) := Cat(io.execOp.din.entryLo(i).pfn, io.execOp.din.entryLo(i).c, io.execOp.din.entryLo(i).d, io.execOp.din.entryLo(i).v)
            .asTypeOf((new TLBEntry).getEntryLoClass())
        }
        entry.g := io.execOp.din.entryLo(0).g & io.execOp.din.entryLo(1).g
      }
    }
    is (tlbwr.U) {
      when (io.execOp.din.index.index < TLBSize.U) {
        val entry = entries(io.execOp.din.index.index)
        entry.pageMask := io.execOp.din.pageMask.mask
        entry.entryHi := Cat(io.execOp.din.entryHi.vpn2, io.execOp.din.entryHi.asid).asTypeOf((new TLBEntry).getEntryHiClass())
        for (i <- 0 until 2) {
          entry.entryLo(i) := Cat(io.execOp.din.entryLo(i).pfn, io.execOp.din.entryLo(i).c, io.execOp.din.entryLo(i).d, io.execOp.din.entryLo(i).v)
            .asTypeOf((new TLBEntry).getEntryLoClass())
        }
        entry.g := io.execOp.din.entryLo(0).g & io.execOp.din.entryLo(1).g
      }
    }
  }

  // address translate
  /**
   * MIPS hardwired memory layout:
   *    0xc0000000 - 0xffffffff   kseg2 (kernel, tlb-mapped)
   *    0xa0000000 - 0xbfffffff   kseg1 (kernel, unmapped, uncached) -> 0x00000000 - 0x1ffffffff
   *    0x80000000 - 0x9fffffff   kseg0 (kernel, unmapped, cached)   -> 0x00000000 - 0x1ffffffff
   *    0x00000000 - 0x7fffffff   kuseg (user, tlb-mapped)
   */
  def direct_map(j: Int) = {
    io.addrTransl(j).phys_addr := io.addrTransl(j).virt_addr
    io.addrTransl(j).isFind := true.B
    io.addrTransl(j).exp := 0.U.asTypeOf(new TLBExceptIO)
  }
  val timeToUcore = RegInit(false.B)
  timeToUcore := Mux(io.addrTransl(0).virt_addr(31, 28) === 0x8.U, true.B, timeToUcore)
  if (enableTLBAddrTransl) {
    for (j <- 0 until port) {
      if (enableItlbAddrTransl && (j == 0 || j == 1) || j == 2) {
        when(timeToUcore) {
          when (io.addrTransl(j).virt_addr(31, 30) =/= "b10".U) {
            io.addrTransl(j).isFind := false.B
            val entryIdx = Wire(UInt(n.W))
            entryIdx := 0.U
            // TODO: complete mask
            for(i <- 0 until TLBSize) {
              when ((entries(i).entryHi.vpn2 === io.addrTransl(j).virt_addr(31, 13))
                && (entries(i).g.asBool() || entries(i).entryHi.asid === io.execOp.din.entryHi.asid)) {
                io.addrTransl(j).isFind := true.B
                entryIdx := i.U
              }
            }

            val pfn = Wire(UInt(PFNSize.W))
            val c = Wire(UInt(3.W))
            val d = Wire(UInt(1.W))
            val v = Wire(UInt(1.W))

            // TODO: use MUX to simplify io.addrTransl(j).exp
            io.addrTransl(j).exp.expType := 0.U
            io.addrTransl(j).exp.expVec := false.B
            io.addrTransl(j).exp.badVaddr := 0.U
            io.addrTransl(j).exp.vpn := 0.U
            when (io.addrTransl(j).isFind) {
              when(io.addrTransl(j).virt_addr(12).asBool()) { // odd
                pfn := entries(entryIdx).entryLo(1).pfn
                c := entries(entryIdx).entryLo(1).c
                d := entries(entryIdx).entryLo(1).d
                v := entries(entryIdx).entryLo(1).v
              } .otherwise {
                pfn := entries(entryIdx).entryLo(0).pfn
                c := entries(entryIdx).entryLo(0).c
                d := entries(entryIdx).entryLo(0).d
                v := entries(entryIdx).entryLo(0).v
              }
              when(!v.asBool()) {
                // SignalException(TLBInvalid, reftype)
                io.addrTransl(j).exp.expType := Mux(io.addrTransl(j).refType === store.U, TLBExceptType.tlbs, TLBExceptType.tlbl)
                io.addrTransl(j).exp.badVaddr := io.addrTransl(j).virt_addr
                io.addrTransl(j).exp.vpn := io.addrTransl(j).virt_addr(31, 13)
              } .elsewhen (!d.asBool() && io.addrTransl(j).refType === store.U(REFTYPE_SIZE.W)) {
                // TODO: SignalException(TLBModified)
                io.addrTransl(j).exp.expType := TLBExceptType.mod
                io.addrTransl(j).exp.badVaddr := io.addrTransl(j).virt_addr
                io.addrTransl(j).exp.vpn := io.addrTransl(j).virt_addr(31, 13)
              }
            } .otherwise {
              pfn := 0.U(PFNSize.W)
              c := 0.U(3.W)
              d := 0.U
              v := 0.U
              // SignalException(TLBMiss, reftype)
              io.addrTransl(j).exp.expType := Mux(io.addrTransl(j).refType === store.U, TLBExceptType.tlbs, TLBExceptType.tlbl)
              io.addrTransl(j).exp.badVaddr := io.addrTransl(j).virt_addr
              io.addrTransl(j).exp.vpn := io.addrTransl(j).virt_addr(31, 13)
              io.addrTransl(j).exp.expVec := true.B // use expvec = 0xbfc00200
            }

            val page_offset = io.addrTransl(j).virt_addr(11, 0)
            io.addrTransl(j).phys_addr := Cat(pfn, page_offset)
          } .otherwise {
            direct_map(j)
          }
        } .otherwise {
          direct_map(j)
        }
      } else {
        direct_map(j)
      }
    }
  } else {
    for (j <- 0 until port) {
      direct_map(j)
    }
  }


}
