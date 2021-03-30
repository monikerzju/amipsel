package isa

import chisel3._ 
import chisel3.util._ 
import conf.Config

trait MicroOpCtrl {
  val SZ_MICRO_OP = 1
}

class DecIO extends Bundle with Config with MicroOpCtrl {
  val inst = Input(UInt(len.W))
  val mops = Output(UInt(SZ_MICRO_OP.W))
}

class Dec extends Module with MicroOpCtrl {
  val io = IO(new DecIO)
}