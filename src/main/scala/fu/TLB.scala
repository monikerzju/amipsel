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
  val tlbp = 0
  val tlbr = 1
  val tlbwi = 2
  val TLBOPTYPE_SIZE = 2
}

class TLBEntryIO extends Bundle with Config {
  val entryHi = Output(new EntryHiStruct)
  val entryLo = Output(Vec(2, new EntryLoStruct))
  val pageMask = Output(new PageMaskStruct)
  val index = Output(new IndexStruct)
}

class TLBIO extends Bundle with Config with RefType with TLBOpType {
  val refType = Input(UInt(REFTYPE_SIZE.W))
  val virt_addr = Input(UInt(len.W))
  val phys_addr = Output(UInt(len.W))
  val op = Input(UInt(TLBOPTYPE_SIZE.W))
  val din = Flipped(new TLBEntryIO)
  val dout = new TLBEntryIO
  val isFind = Output(Bool())
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

class TLB extends Module with Config with RefType with TLBOpType {
  val io = IO(new TLBIO)
  // TODO: check TLB initialize
  val entries = Reg(Vec(TLBSize ,new TLBEntry))

  io.isFind := false.B
  var entryIdx: Int = -1

  // TODO: complete mask
  for(i <- 0 until TLBSize) {
    when ((entries(i).entryHi.vpn2 === io.virt_addr(31, 13))
      && (entries(i).g.asBool() || entries(i).entryHi.asid === io.din.entryHi.asid)) {
      io.isFind := true.B
      entryIdx = i
    }
  }

  val pfn = Wire(UInt(PFNSize.W))
  val c = Wire(UInt(3.W))
  val d = Wire(UInt(1.W))
  val v = Wire(UInt(1.W))

  when (io.isFind) {
    when(io.virt_addr(12).asBool()) { // odd
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
      // TODO: SignalException(TLBInvalid, reftype)
    } .elsewhen (!d.asBool() && io.refType === store.U(REFTYPE_SIZE.W)) {
      // TODO: SignalException(TLBModified)
    }
  } .otherwise {
    pfn := 0.U(PFNSize.W)
    c := 0.U(3.W)
    d := 0.U
    v := 0.U
    // TODO: SignalException(TLBMiss, reftype)

  }

  val page_offset = io.virt_addr(11, 0)
  io.phys_addr := Cat(pfn, page_offset)

  io.dout := 0.U((new TLBEntryIO).getWidth.W).asTypeOf(new TLBEntryIO)
  switch (io.op) {
    is (tlbp.U) {
      var idx: Int = -1
      for(i <- 0 until TLBSize) {
        when ((entries(i).entryHi.vpn2 === io.din.entryHi.vpn2)
          && (entries(i).g.asBool() || entries(i).entryHi.asid === io.din.entryHi.asid)) {
          idx = i
        }
      }
      val n = log2Ceil(TLBSize + 1)
      if (idx == -1) { // not find entry
        io.dout.index := Cat(1.U(1.W), 0.U((len - 1).W)).asTypeOf(new IndexStruct)
      } else {
        io.dout.index := Cat(0.U((len - n).W), idx.U(n.W)).asTypeOf(new IndexStruct)
      }
    }
    is (tlbr.U) {
      when (io.din.index.index < TLBSize.U) {
        val entry = entries(io.din.index.index)
        io.dout.pageMask := Cat(0.U(7.W), entry.pageMask, 0.U(13.W)).asTypeOf(new PageMaskStruct)
        io.dout.entryHi := Cat(entry.entryHi.vpn2, 0.U(5.W), entry.entryHi.asid).asTypeOf(new EntryHiStruct)
        io.dout.entryLo(0) := Cat(0.U(6.W), entry.entryLo(0).asUInt(), entry.g).asTypeOf(new EntryLoStruct)
        io.dout.entryLo(1) := Cat(0.U(6.W), entry.entryLo(1).asUInt(), entry.g).asTypeOf(new EntryLoStruct)
      }
    }
    is (tlbwi.U) {
      when (io.din.index.index < TLBSize.U) {
        val entry = entries(io.din.index.index)
        entry.pageMask := io.din.pageMask.mask
        entry.entryHi := Cat(io.din.entryHi.vpn2, io.din.entryHi.asid).asTypeOf((new TLBEntry).getEntryHiClass())
        for (i <- 0 until 2) {
          entry.entryLo(i) := Cat(io.din.entryLo(i).pfn, io.din.entryLo(i).c, io.din.entryLo(i).d, io.din.entryLo(i).v)
            .asTypeOf((new TLBEntry).getEntryLoClass())
        }
        entry.g := io.din.entryLo(0).g & io.din.entryLo(1).g
      }
    }
  }
}
