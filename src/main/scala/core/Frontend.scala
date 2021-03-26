package core

import chisel3._
import chisel3.util._

class Frontend extends Module with Config {
  val io = IO(new FrontendIO)
}