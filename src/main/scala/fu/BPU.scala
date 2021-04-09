// TODO RAS and BS
// DONE BTBv1

package fu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import cache.BRAMSyncReadMem 

class RAS(depth: Int = 8, width: Int = 32) {
  val count = RegInit(0.U(log2Up(depth + 1).W))
  val pos = RegInit(0.U(log2Up(depth).W))
  val stack = Reg(Vec(depth, UInt(width.W)))

  def push(addr: UInt): Unit = {
    when (count < depth.U) { count := count + 1.U }
    val next_pos = Mux(isPow2(depth).asBool() || pos < (depth - 1).U, pos + 1.U, 0.U)
    stack(next_pos) := addr
    pos := next_pos
  }
  def peek: UInt = stack(pos)
  def pop(): Unit = when (!isEmpty) {
    count := count - 1.U
    pos := Mux(isPow2(depth).asBool() || pos > 0.U, pos - 1.U, (depth - 1).U)
  }
  def clear(): Unit = count := 0.U
  def isEmpty: Bool = count === 0.U
}

// First, the delay slot will cause multiple issues.
// So I try BHT[DS-Addr] = Jump-Or-Not in order to get rid of waiting the vilatile delay slot fetching operation.
// Furthermore, BRAM has 1 cycle delay, so I have to predict the next insturction rather than the original one.
// Thanks to the delay slot, the next-instrcution prediction will bring out no penalty even if the first instruction is JMP.
class BHTReq(width: Int = 32) extends Bundle {
  val next_line = Input(UInt(width.W))
}

class BHTResp(width: Int = 32, issueN: Int = 2) extends Bundle {
  val taken_vec = Output(Vec(issueN, Bool()))
  val target_vec = Output(Vec(issueN, UInt(width.W)))
}

class BHTExeUpdate(width: Int = 32) extends Bundle {
  val v = Input(Bool())
  val errpr = Input(Bool())          // wrong 1, reinforce 0
  val pc_ds = Input(UInt(width.W))   // bht[pc_ds] will change
  val target = Input(UInt(width.W))
  val taken = Input(Bool())
}

// If predict taken but not a branch instruction, CPU will not jump after the so-called branch slot is fetched.
// So, the pipeline needs flushing
class BHTDecUpdate(width: Int = 32, decodeFuN: Int = 2) extends Bundle {
  val v_vec = Input(Vec(decodeFuN, Bool()))
  val pc_ds = Input(Vec(decodeFuN, UInt(width.W)))   // bht[pc_ds] to not taken, the original state
}

class BHTUpdate(width: Int = 32, decodeFuN: Int = 2) extends Bundle {
  val dec = new BHTDecUpdate(width, decodeFuN)
  val exe = new BHTExeUpdate(width)
}

class RASReq() extends Bundle {

}

class RASResp() extends Bundle {

}

// req.nextline = pc + 8
// resp.taken_vec(0) is for pc, (1) is for pc + 4, this signal also influence how many instructions to decode
class BPUIO(width: Int = 32, issueN: Int = 2) extends Bundle {
  val req = new BHTReq(width)
  val resp = new BHTResp(width)
  val update = new BHTUpdate(width, issueN)
}

// DS-oriented programming
class BPU(depth: Int = 256, offset: Int = 3, width: Int = 32, issueN: Int = 2, rasDepth: Int = 0) extends Module {
  val io = IO(new BPUIO(width, issueN))

  val SN = 0

  // BHT are registers, because it is relatively small and BRAM does not support reset
  val history = RegInit(VecInit(Seq.fill(depth)(SN.U(2.W))))  // half a regfile's size
  val buffer = Module(new BRAMSyncReadMem(depth, width, 1))

  // 0 for low addr, 1 for high addr
  for (i <- 0 until issueN) {
    io.resp.taken_vec(i) := RegNext(history(io.req.next_line + (i.U << 2.U))(1))  // 10 and 11 for WT and ST
    if (i == 0)
      io.resp.target_vec(0) := buffer.io.douta
    else
      io.resp.target_vec(i) := buffer.io.doutb
  }

  // Notice that the update from ID will only change BHT
  buffer.io.wea := io.update.exe.errpr
  buffer.io.web := false.B
  buffer.io.addra := Mux(io.update.exe.errpr, io.update.exe.pc_ds, io.req.next_line)
  buffer.io.addrb := io.req.next_line + 4.U
  buffer.io.dina := io.update.exe.target
  buffer.io.dinb := DontCare
  
  // HT update, do not care the prediction because when the prediction is wrong, the pipeline shall be flushed anyway
  // ID's priority is higher than EX
  when (io.update.exe.v) {
    val old_value = history(io.update.exe.pc_ds(offset + log2Ceil(depth), offset))
    history(io.update.exe.pc_ds(offset + log2Ceil(depth), offset)) := Mux(
      io.update.exe.taken,
      Mux(old_value.andR, old_value, old_value + 1.U),
      Mux(!old_value.orR, old_value, old_value - 1.U)
    )
  }
  for (i <- 0 until issueN) {
    when (io.update.dec.v_vec(i)) {
      history(io.update.dec.pc_ds(i)(offset + log2Ceil(depth), offset)) := SN.U
    }
  }

  // RAS is ds-oriented and done in ID stage
  if (rasDepth > 0) {
    val ras = new RAS(rasDepth, width)
  }
}
