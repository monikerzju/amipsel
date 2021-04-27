package fu

import chisel3._
import chisel3.util._ 
import conf.Config 

trait CP0Code {
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

  val StatusWMask = "b00000000000000001111111100000011"
}

trait CauseExcCode {
  val Interrupt    = 0x0
  val TLBModify    = 0x1
  val TLBLoad      = 0x2
  val TLBStore     = 0x3
  val AddrErrLoad  = 0x4
  val AddrErrStore = 0x5
  val Syscall      = 0x8
  val Breakpoint   = 0x9
  val ReservedInst = 0xa
  val Overflow     = 0xc
  val Trap         = 0xd

  val SZ_EXC_CODE  = Trap + 1
  val SZ_HARD_INT  = 6
  val SZ_SOFT_INT  = 2
  
  val ExceptPriority = Seq(
    Interrupt,
    AddrErrLoad,
    ReservedInst,
    Overflow,
    Trap,
    Syscall,
    AddrErrStore
  )
}

class FromToCIO extends Bundle with Config with CP0Code {
  val wen = Input(Bool())
  val code = Input(UInt(SZ_CP0_CODE.W))
  val sel = Input(UInt(SZ_CP0_SEL.W))
  val din = Input(UInt(len.W))
  val dout = Output(UInt(len.W))
}

class ExceptIO extends Bundle with Config with CauseExcCode {
  val valid_inst = Input(Bool())
  val except_vec = Input(Vec(SZ_EXC_CODE, Bool()))
  val hard_int_vec = Input(Vec(SZ_HARD_INT, Bool()))
  val ret = Input(Bool())
  val epc = Input(UInt(len.W))
  val in_delay_slot = Input(Bool())
  val bad_addr = Input(UInt(len.W))
  val except_kill = Output(Bool())
  val except_redirect = Output(UInt(len.W))
}

// There might be other IO, but we will use BoringUtils.
class CP0IO extends Bundle with Config {
  val ftc = new FromToCIO
  val except = new ExceptIO
}

class StatusStruct extends Bundle {
  val res0 = Output(UInt((31-23+1).W))
  val bev  = Output(UInt(1.W))
  val res1 = Output(UInt((21-16+1).W))
  val im   = Output(UInt((15-8+1).W))
  val res2 = Output(UInt((7-2+1).W))
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

// Put CP0 in WB stage anyway
class CP0 extends Module with CP0Code with CauseExcCode with Config {
  val io = IO(new CP0IO)

  val badvaddrr = Reg(UInt(len.W))
  val countr = Reg(UInt((len + 1).W))
  val comparer = Reg(UInt(len.W))
  val statusr = RegInit(statusVal.U(len.W))
  val causer = RegInit(0.U(len.W))
  val epcr = Reg(UInt(len.W))

  val tim_int = RegInit(false.B)
  when (countr === comparer) {
    tim_int := true.B
  }.elsewhen (io.ftc.wen && io.ftc.code === Compare.U) {
    tim_int := false.B
  }
  val int_en = (
    !statusr.asTypeOf(new StatusStruct).exl &&
    statusr.asTypeOf(new StatusStruct).ie.asBool &&
    (Cat(io.except.hard_int_vec.asUInt | tim_int << 5.U,
    causer.asTypeOf(new CauseStruct).ips) & 
    (~ statusr.asTypeOf(new StatusStruct).im)).orR
  )
  val real_except_vec = Vec(SZ_EXC_CODE, Bool())
  for (i <- 0 until SZ_EXC_CODE) {
    if (i != Interrupt)
      real_except_vec(i) := io.except.except_vec(i)
    else
      real_except_vec(Interrupt) := io.except.except_vec(Interrupt) && int_en
  }
  val has_except = real_except_vec.asUInt.orR && io.except.valid_inst
  val except_code = ExceptPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(real_except_vec(i), i.U, sum))
  val ret = io.except.ret

  io.ftc.dout := badvaddrr
  switch (io.ftc.code) {
    is (BadVAddr.U) { io.ftc.dout := badvaddrr                                                                        }
    is (Count.U)    { io.ftc.dout := countr(len, 1)                                                                   }
    is (Status.U)   { io.ftc.dout := statusr                                                                          }
    is (Cause.U)    { io.ftc.dout := Cat(false.B, tim_int, causer(29, 16), io.except.except_vec.asUInt, causer(9, 0)) }
    is (EPC.U)      { io.ftc.dout := epcr                                                                             }
    is (Compare.U)  { io.ftc.dout := comparer                                                                         }
  }

  io.except.except_kill := has_except || ret
  io.except.except_redirect := Mux(ret, epcr, trapAddr.U)
  countr := countr + 1.U
  when (has_except) {
    val new_status = WireInit(statusr.asTypeOf(new StatusStruct))
    new_status.exl := 1.U
    statusr := new_status.asUInt
    val new_cause = WireInit(causer.asTypeOf(new CauseStruct))
    new_cause.bd := Mux(io.except.in_delay_slot, 1.U, 0.U)
    new_cause.exc := except_code
    when (statusr.asTypeOf(new StatusStruct).exl === 0.U) {
      causer := new_cause
      epcr := Mux(io.except.in_delay_slot, io.except.epc - 4.U, io.except.epc)
    }
    badvaddrr := Mux(except_code === AddrErrLoad.U || except_code === AddrErrStore.U, io.except.bad_addr, badvaddrr)
  }.elsewhen (ret) {
    val new_status = WireInit(statusr.asTypeOf(new StatusStruct))
    new_status.exl := 0.U
    statusr := new_status.asUInt
  }.elsewhen (io.ftc.wen) {
    switch (io.ftc.code) {
      // BadVAddr is unwritable
      is (Count.U)    { countr := Cat(io.ftc.din, 0.U(1.W))                                      }
      is (Status.U)   { statusr := (statusr & (~StatusWMask.U)) | (io.ftc.din & StatusWMask.U)   }
      is (Cause.U)    { io.ftc.dout := Cat(causer(31, 10), io.ftc.din(9, 8), causer(7, 0))       }
      is (EPC.U)      { epcr := io.ftc.din                                                       }
      is (Compare.U)  { comparer := io.ftc.din                                                   }
    }
  }
}
