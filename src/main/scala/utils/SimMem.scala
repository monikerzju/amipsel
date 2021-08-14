package utils

import chisel3._
import chisel3.util._
import chisel3.util.experimental._

import cache._
import icore._
import conf.Config

class SimMem extends Module with Config with MemAccessType {
  val io = IO(new Bundle {
    val icache_io = Flipped(new CacheIO(1 << (offsetBits + 3)))
    val dcache_io = Flipped(new CacheIO(1 << (offsetBits + 3)))
  })

  val write_ram = WireDefault(true.B)

  val ram_mask = "h3ffffff".U
  val icandidates = Wire(Vec(32, UInt(8.W)))
  val dcandidates = Wire(Vec(32, UInt(8.W)))

  when(io.dcache_io.req.valid) {
    when(io.dcache_io.req.addr(29, 0) >= "h4000000".U) {
      write_ram := false.B
      // printf("dcache is accessing %x, might be mmio\n", io.dcache_io.req.addr)
    }
    when(io.dcache_io.req.addr(28, 0) === "h1fd003f8".U && io.dcache_io.req.wen && !io.dcache_io.resp.valid) {
      // printf("%c", io.dcache_io.req.data(7, 0))
    }
  }

  when(io.icache_io.req.valid) {
    when(io.icache_io.req.addr >= "h84000000".U) {
      // printf("icache is accessing %x, ram overflow\n", io.icache_io.req.addr)
    }
    // printf("icache reading %x, inst is %x\n", io.icache_io.req.addr, icandidates(0))
  }

  // For simplicity, fixed at 0x8000_0000 to 0x8400_0000 RAM, 64MB
  // async read, sync write
  val memory = Mem(0x4000000L, UInt(8.W))
  loadMemoryFromFileInline(memory, "../elf/ucore-kernel-initrd.hex")

  for (i <- 0 until 32) {
    icandidates(i) := memory.read(ram_mask & (io.icache_io.req.addr + i.U))
    dcandidates(i) := memory.read(ram_mask & (Cat(io.dcache_io.req.addr(31,2),0.U(2.W)) + i.U))
  }
  when(io.dcache_io.req.valid && io.dcache_io.req.addr(31, 0) >= "ha0000000".U) {
    write_ram := false.B
    dcandidates(0) := "hdeadbeef".U
    switch(io.dcache_io.req.addr){  // linux only
      is("hbfd003fd".U){  for(i <- 0 until 8)dcandidates(i) := "h60".U }
      is("hbfd00071".U){  for(i <- 0 until 8)dcandidates(i) := "hff".U }  // refered by pc = 80192864
    }
    printf("%c",io.dcache_io.req.data(7,0))
    // printf("dcache is accessing %x, might be mmio\n", io.dcache_io.req.addr)
    // printf("returning %x\n",dcandidates.asUInt)
  }
//   printf("mem 0x80000000 is %x\n", Cat(memory.read(3.U), memory.read(2.U), memory.read(1.U), memory.read(0.U)))

  // read is simple
  io.icache_io.resp.valid := RegNext(io.icache_io.req.valid)
  io.icache_io.resp.data  := RegNext(icandidates.asUInt)
  io.dcache_io.resp.valid := RegNext(io.dcache_io.req.valid)
  io.dcache_io.resp.data  := RegNext(dcandidates.asUInt)

  // write is complex
  val uart_print_addr = 0
  when(io.dcache_io.req.wen && write_ram) {
    switch(io.dcache_io.req.mtype) {
      is(MEM_BYTE.U) {
        // 1 byte
        memory.write(io.dcache_io.req.addr & ram_mask, io.dcache_io.req.data(7, 0))
      }
      is(MEM_HALF.U) {
        // 2 byte
        memory.write(io.dcache_io.req.addr & ram_mask, io.dcache_io.req.data(7, 0))
        memory.write((io.dcache_io.req.addr + 1.U) & ram_mask, io.dcache_io.req.data(15, 8))
      }
      is(MEM_WORD.U) {
        // 4 byte
        memory.write(io.dcache_io.req.addr & ram_mask, io.dcache_io.req.data(7, 0))
        memory.write((io.dcache_io.req.addr + 1.U) & ram_mask, io.dcache_io.req.data(15, 8))
        memory.write((io.dcache_io.req.addr + 2.U) & ram_mask, io.dcache_io.req.data(23, 16))
        memory.write((io.dcache_io.req.addr + 3.U) & ram_mask, io.dcache_io.req.data(31, 24))
      }
      is(MEM_DWORD.U) {
        // 32 byte
        for(j <- 0 until 32) {
          memory.write((io.dcache_io.req.addr + j.U) & ram_mask, io.dcache_io.req.data((j + 1) * 8 - 1, j * 8))
        }
      }
    }
  }
}