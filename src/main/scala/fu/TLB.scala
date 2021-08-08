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
  val TLBOPTYPE_SIZE = 2
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

  // tlb operation
  io.execOp.dout := 0.U((new TLBEntryIO).getWidth.W).asTypeOf(new TLBEntryIO)
  switch (io.execOp.op) {
    is (tlbp.U) {
      var idx: Int = -1
      for(i <- 0 until TLBSize) {
        when ((entries(i).entryHi.vpn2 === io.execOp.din.entryHi.vpn2)
          && (entries(i).g.asBool() || entries(i).entryHi.asid === io.execOp.din.entryHi.asid)) {
          idx = i
        }
      }
      val n = log2Ceil(TLBSize + 1)
      if (idx == -1) { // not find entry
        io.execOp.dout.index := Cat(1.U(1.W), 0.U((len - 1).W)).asTypeOf(new IndexStruct)
      } else {
        io.execOp.dout.index := Cat(0.U((len - n).W), idx.U(n.W)).asTypeOf(new IndexStruct)
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
  }

  // address translate
  for (j <- 0 until port) {
    io.addrTransl(j).isFind := false.B
    var entryIdx: Int = -1

    // TODO: complete mask
    for(i <- 0 until TLBSize) {
      when ((entries(i).entryHi.vpn2 === io.addrTransl(j).virt_addr(31, 13))
        && (entries(i).g.asBool() || entries(i).entryHi.asid === io.execOp.din.entryHi.asid)) {
        io.addrTransl(j).isFind := true.B
        entryIdx = i
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
        io.addrTransl(j).exp.vpn := io.addrTransl(j).virt_addr(31, 12)
      } .elsewhen (!d.asBool() && io.addrTransl(j).refType === store.U(REFTYPE_SIZE.W)) {
        // TODO: SignalException(TLBModified)
        io.addrTransl(j).exp.expType := TLBExceptType.mod
        io.addrTransl(j).exp.badVaddr := io.addrTransl(j).virt_addr
        io.addrTransl(j).exp.vpn := io.addrTransl(j).virt_addr(31, 12)
      }
    } .otherwise {
      pfn := 0.U(PFNSize.W)
      c := 0.U(3.W)
      d := 0.U
      v := 0.U
      // SignalException(TLBMiss, reftype)
      io.addrTransl(j).exp.expType := Mux(io.addrTransl(j).refType === store.U, TLBExceptType.tlbs, TLBExceptType.tlbl)
      io.addrTransl(j).exp.badVaddr := io.addrTransl(j).virt_addr
      io.addrTransl(j).exp.vpn := io.addrTransl(j).virt_addr(31, 12)
      io.addrTransl(j).exp.expVec := true.B // use expvec = 0xbfc00200
    }

    val page_offset = io.addrTransl(j).virt_addr(11, 0)
    io.addrTransl(j).phys_addr := Cat(pfn, page_offset)
  }

}
