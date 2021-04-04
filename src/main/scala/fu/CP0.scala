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
}

trait CauseExcCode {
  val Interrupt    = 0x0
  val TLBModify    = 0x1
  val TLBLoad      = 0x2
  val TBLStore     = 0x3
  val AddrErrLoad  = 0x4
  val AddrErrStore = 0x5
  val Syscall      = 0x8
  val Breakpoint   = 0x9
  val ReservedInst = 0xa
  val CPUnuseable  = 0xb
  val Overflow     = 0xc
  val Trap         = 0xd
}

class CP0IO extends Bundle with Config {
  
}

class CP0 extends Module with CP0Code with CauseExcCode with Config {
  val io = IO(new CP0IO)
}
