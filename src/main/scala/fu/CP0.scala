package fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import conf.Config

trait CP0Code extends Config {
  val Index    = 0
  val Random   = 1
  val EntryLo0 = 2
  val EntryLo1 = 3
  val Context  = 4
  val PageMask = 5
  val Wired    = 6
  val BadVAddr = 8
  val Count    = 9
  val EntryHi  = 10
  val Compare  = 11
  val Status   = 12
  val Cause    = 13
  val EPC      = 14
  val PRId     = 15
  val Config   = 16
  val Config1  = 16
  val TagLo    = 28

  val SZ_CP0_CODE = log2Ceil(TagLo)
  val SZ_CP0_SEL  = 1

  val StatusWMask  = if (withBigCore) "b00010000000000001111111100000011" else "b00000000000000001111111100000011"
  val nStatusWMask = if (withBigCore) "b11101111111111110000000011111100" else "b11111111111111110000000011111100"
}

trait CauseExcCode {
  val Interrupt    = 0x0
  val TLBModify    = 0x1
  val TLBLoad      = 0x2
  val TLBStore     = 0x3
  val AddrErrLoad  = 0x4
  val AddrErrStore = 0x5
  val Res0         = 0x6
  val Res1         = 0x7
  val Syscall      = 0x8
  val Breakpoint   = 0x9
  val ReservedInst = 0xa
  val Res2         = 0xb
  val Overflow     = 0xc
  val Trap         = 0xd

  val SZ_EXC_CODE  = Trap + 1
  val SZ_HARD_INT  = 6
  val SZ_SOFT_INT  = 2

  // TODO: tlb_exp diff between data access and inst fetch
  val ExceptPriority = Seq(
    Interrupt,
    AddrErrLoad,
    TLBLoad,
    TLBStore,
    TLBModify,
    ReservedInst,
    Overflow,
    Trap,
    Breakpoint,
    Syscall,
    AddrErrStore
  )
}

class FromToCIO extends Bundle with Config with CP0Code {
  val wen  = Input(Bool())
  val code = Input(UInt(SZ_CP0_CODE.W))
  val sel  = Input(UInt(SZ_CP0_SEL.W))
  val din  = Input(UInt(len.W))
  val dout = Output(UInt(len.W))
}

class ExceptIO extends Bundle with Config with CauseExcCode {
  val valid_inst      = Input(Bool())
  val except_vec      = Input(Vec(SZ_EXC_CODE, Bool()))
  val hard_int_vec    = Input(Vec(SZ_HARD_INT, Bool()))
  val ret             = Input(Bool())
  val epc             = Input(UInt(len.W))
  val in_delay_slot   = Input(Bool())
  val bad_addr        = Input(UInt(len.W))
  val resp_for_int    = Input(Bool())
  val call_for_int    = Output(Bool())
  val except_kill     = Output(Bool())
  val except_redirect = Output(UInt(len.W))
}

class FromToTlb extends Bundle with Config {
  val exec = new TLBOpIO
  val vpn = Input(UInt(VPNSize.W))
  val expVec = Input(Bool())
}

// There might be other IO, but we will use BoringUtils.
class CP0IO extends Bundle with Config {
  val ftc = new FromToCIO
  val except = new ExceptIO
  val ftTlb = if (withBigCore) new FromToTlb else null
}

class StatusStruct extends Bundle with Config {
  val res0 = if (withBigCore) Output(UInt((31-27+1).W)) else Output(UInt((31-28+1).W))
  val cu0  = if (withBigCore) Output(UInt(1.W)) else null
  val res1 = Output(UInt((27-23+1).W))
  val bev  = Output(UInt(1.W))
  val res2 = Output(UInt((21-16+1).W))
  val im   = Output(UInt((15-8+1).W))
  val res3 = Output(UInt((7-2+1).W))
  val exl  = Output(UInt(1.W))
  val ie   = Output(UInt(1.W))
}

class CauseStruct extends Bundle {
  val bd   = Output(UInt(1.W))
  val ti   = Output(UInt(1.W))
  val res0 = Output(UInt((29-16+1).W))
  val iph  = Output(UInt((15-10+1).W))
  val ips  = Output(UInt((9-8+1).W))
  val res1 = Output(UInt(1.W))
  val exc  = Output(UInt((6-2+1).W))
  val res2 = Output(UInt(2.W))
}

class EntryHiStruct extends Bundle {
  val vpn2 = Output(UInt((31 - 13 + 1).W))
  val const0 = Output(UInt((12 - 8 + 1).W))
  val asid = Output(UInt(8.W))
}

class EntryLoStruct extends Bundle {
  val const0 = Output(UInt((31 - 26 + 1).W))
  val pfn = Output(UInt((25 - 6 + 1).W))
  val c = Output(UInt((5 - 3 + 1).W))
  val d = Output(UInt(1.W))
  val v = Output(UInt(1.W))
  val g = Output(UInt(1.W))
}

class PageMaskStruct extends Bundle {
  val const0_0 = Output(UInt((31 - 25 + 1).W))
  val mask = Output(UInt((24 - 13 + 1).W))
  val const0_1 = Output(UInt(13.W))
}

class IndexStruct extends Bundle with Config {
  val p = Output(UInt(1.W))
  val const0 = Output(UInt((30 - log2Up(TLBSize) + 1).W))
  val index = Output(UInt(log2Up(TLBSize).W))
}

class RandomStruct extends Bundle with Config {
  val const0 = Output(UInt((len - log2Up(TLBSize) + 1).W))
  val random = Output(UInt(log2Up(TLBSize).W))
}

// Put CP0 in WB stage anyway
class CP0(diffTestV: Boolean = false) extends Module with CP0Code with CauseExcCode with Config with TLBOpType {
  val io = IO(new CP0IO)

  val badvaddrr = Reg(UInt(len.W))
  val countr    = RegInit(0.U((len + 1).W))
  val comparer  = RegInit("hffffffff".U(len.W))
  val statusr   = RegInit(statusVal.U(len.W))
  val causer    = RegInit(0.U(len.W))
  val epcr      = Reg(UInt(len.W))

  val entryHir  = if (withBigCore) RegInit(0.U(len.W)) else null
  val entryLor  = if (withBigCore) RegInit(VecInit(Seq.fill(2)(0.U(len.W)))) else null
  val pageMaskr = if (withBigCore) RegInit(0.U(len.W)) else null
  val indexr    = if (withBigCore) RegInit(0.U(len.W)) else null
  val randomr   = if (withBigCore) RegInit((TLBSize - 1).U(len.W)) else null // do not implement Wired cp0

  if (withBigCore) {
    if (useQEMURandomStrategy) {
      val prev_randomr = RegInit(0.U(len.W))
      val seed = RegInit(1.U(len.W))
      def getNextSeed(s: UInt): UInt = {
        1103515245.U * s + 12345.U
      }
      def getRandom(s: UInt): UInt = {
        // qemu 4.2 implementation
        (getNextSeed(s) >> 16).asUInt() % TLBSize.U
      }
      // TODO: avoid stall influence the seed
      when(io.ftTlb.exec.op === tlbwr.U || io.ftc.code === Random.U) {
        // omit third or more getRandom return the same value as before
        seed := Mux(getRandom(seed) === prev_randomr, getNextSeed(getNextSeed(seed)), getNextSeed(seed))
        randomr := Mux(getRandom(seed) === prev_randomr, getRandom(getNextSeed(seed)), getRandom(seed))
        prev_randomr := randomr
      }
    } else {
      randomr := randomr - 1.U // simple random strategy
    }

    io.ftTlb.exec.dout := {
      val entry = Wire(new TLBEntryIO)
      entry.entryHi  := entryHir.asTypeOf(new EntryHiStruct)
      entry.pageMask := pageMaskr.asTypeOf(new PageMaskStruct)
      entry.index    := Mux(io.ftTlb.exec.op === tlbwr.U, randomr.asTypeOf(new IndexStruct), indexr.asTypeOf(new IndexStruct))
      for (i <- 0 until 2) {
        entry.entryLo(i) := entryLor(i).asTypeOf(new EntryLoStruct)
      }
      entry
    }

    switch (io.ftTlb.exec.op) {
      is (tlbr.U) {
        entryHir     := io.ftTlb.exec.din.entryHi.asUInt()
        entryLor(0)  := io.ftTlb.exec.din.entryLo(0).asUInt()
        entryLor(1)  := io.ftTlb.exec.din.entryLo(1).asUInt()
        pageMaskr    := io.ftTlb.exec.din.pageMask.asUInt()
      }
      is (tlbp.U) {
        indexr := io.ftTlb.exec.din.index.asUInt()
      }
    }
  }

  val tim_int = RegInit(false.B)
  when (io.ftc.wen && io.ftc.code === Compare.U && io.except.valid_inst) {
    tim_int := false.B
  }.elsewhen (countr === comparer) {
    tim_int := true.B
  }
  val real_hard_int_vec = io.except.hard_int_vec.asUInt | Cat(tim_int, Fill(5, 0.U))
  val int_en = (
    !statusr.asTypeOf(new StatusStruct).exl &&
    statusr.asTypeOf(new StatusStruct).ie.asBool &&
    (Cat(real_hard_int_vec,
    causer.asTypeOf(new CauseStruct).ips) &
    statusr.asTypeOf(new StatusStruct).im.asUInt).orR
  )
  val real_except_vec = Wire(Vec(SZ_EXC_CODE, Bool()))
  val has_except = real_except_vec.asUInt.orR && io.except.valid_inst
  val except_code = ExceptPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(real_except_vec(i), i.U, sum))
  val ret = io.except.ret && io.except.valid_inst
  val error_ret = ret && epcr(1, 0).orR
  for (i <- 0 until SZ_EXC_CODE) {
    if (i != Interrupt && i != AddrErrLoad)
      real_except_vec(i) := io.except.except_vec(i)
    else if (i == AddrErrLoad)
      real_except_vec(AddrErrLoad) := io.except.except_vec(AddrErrLoad) || error_ret
    else
      real_except_vec(Interrupt) := int_en && io.except.resp_for_int
  }

  io.ftc.dout := badvaddrr
  val read_causer = Cat(causer(len - 1), tim_int, causer(29, 16), real_hard_int_vec, causer(9, 0))
  val countw = if (diffTestV) 0.U else countr(len, 1)
  if (withBigCore) {
    switch (io.ftc.code) {
      is (BadVAddr.U) {   io.ftc.dout := badvaddrr      }
      is (Count.U)    {   io.ftc.dout := countw         }
      is (Status.U)   {   io.ftc.dout := statusr        }
      is (Cause.U)    {   io.ftc.dout := read_causer    }
      is (EPC.U)      {   io.ftc.dout := epcr           }
      is (Compare.U)  {   io.ftc.dout := comparer       }
      is (EntryHi.U)  {   io.ftc.dout := entryHir       }
      is (EntryLo0.U) {   io.ftc.dout := entryLor(0)    }
      is (EntryLo1.U) {   io.ftc.dout := entryLor(1)    }
      is (PageMask.U) {   io.ftc.dout := pageMaskr      }
      is (Index.U)    {   io.ftc.dout := indexr         }
      is (Random.U)   {   io.ftc.dout := randomr        }
    }
  } else {
    switch (io.ftc.code) {
      is (BadVAddr.U) {   io.ftc.dout := badvaddrr      }
      is (Count.U)    {   io.ftc.dout := countw         }
      is (Status.U)   {   io.ftc.dout := statusr        }
      is (Cause.U)    {   io.ftc.dout := read_causer    }
      is (EPC.U)      {   io.ftc.dout := epcr           }
      is (Compare.U)  {   io.ftc.dout := comparer       }
    }    
  }


  if (withBigCore) {
    io.except.except_redirect := Mux(ret && !error_ret, epcr, Mux(io.ftTlb.expVec, tlbTrapAddr.U, trapAddr.U))
  } else {
    io.except.except_redirect := Mux(ret && !error_ret, epcr, trapAddr.U)
  }
  io.except.except_kill     := has_except || ret
  io.except.call_for_int    := int_en
  countr := countr + 1.U
  when (has_except || error_ret) {
    val new_status = WireInit(statusr.asTypeOf(new StatusStruct))
    new_status.exl := 1.U
    statusr := new_status.asUInt
    val new_cause = WireInit(causer.asTypeOf(new CauseStruct))
    new_cause.bd := Mux(error_ret, 0.U, Mux(io.except.in_delay_slot, 1.U, 0.U))
    new_cause.exc := Mux(error_ret, AddrErrLoad.U, except_code)
    when (statusr.asTypeOf(new StatusStruct).exl === 0.U || error_ret) {
      causer := new_cause.asUInt
      epcr := Mux(error_ret, epcr, Mux(io.except.in_delay_slot, io.except.epc - 4.U, io.except.epc))
    }
    if (withBigCore) {
      badvaddrr := Mux(error_ret, epcr,
        Mux(except_code === AddrErrLoad.U || except_code === AddrErrStore.U || except_code === TLBLoad.U || except_code === TLBStore.U,
          io.except.bad_addr, badvaddrr))
      entryHir := Mux(except_code === TLBLoad.U || except_code === TLBStore.U, Cat(io.ftTlb.vpn, entryHir(12, 0)), entryHir)
    } else {
      badvaddrr := Mux(error_ret, epcr, Mux(except_code === AddrErrLoad.U || except_code === AddrErrStore.U, io.except.bad_addr, badvaddrr))
    }
  }.elsewhen (ret) {
    val new_status = WireInit(statusr.asTypeOf(new StatusStruct))
    new_status.exl := 0.U
    statusr := new_status.asUInt
  }.elsewhen (io.ftc.wen && io.except.valid_inst) {
    val status_imut = statusr & nStatusWMask.U
    val status_mut  = io.ftc.din & StatusWMask.U
    if (withBigCore) {
      val n = log2Up(TLBSize)
      switch (io.ftc.code) {
        // BadVAddr is unwritable
        is (Count.U)    {   countr := Cat(io.ftc.din, 0.U(1.W))                                }
        is (Status.U)   {   statusr := status_imut | status_mut                                }
        is (Cause.U)    {   causer := Cat(causer(31, 10), io.ftc.din(9, 8), causer(7, 0))      }
        is (EPC.U)      {   epcr := io.ftc.din                                                 }
        is (Compare.U)  {   comparer := io.ftc.din                                             }
        is (EntryHi.U)  {   entryHir := Cat(io.ftc.din(31, 13), 0.U(5.W), io.ftc.din(7, 0))    }
        is (EntryLo0.U) {   entryLor(0) := Cat(0.U(6.W), io.ftc.din(25, 0))                    }
        is (EntryLo1.U) {   entryLor(1) := Cat(0.U(6.W), io.ftc.din(25, 0))                    }
        is (PageMask.U) {   pageMaskr := Cat(0.U(7.W), io.ftc.din(24, 13), 0.U(13.W))          }
        is (Index.U)    {   indexr := Cat(indexr(len - 1, n), io.ftc.din(n - 1, 0))            }  
      }
    } else {
      switch (io.ftc.code) {
        // BadVAddr is unwritable
        is (Count.U)    {   countr := Cat(io.ftc.din, 0.U(1.W))                                }
        is (Status.U)   {   statusr := status_imut | status_mut                                }
        is (Cause.U)    {   causer := Cat(causer(31, 10), io.ftc.din(9, 8), causer(7, 0))      }
        is (EPC.U)      {   epcr := io.ftc.din                                                 }
        is (Compare.U)  {   comparer := io.ftc.din                                             } 
      }      
    }
  }
}
