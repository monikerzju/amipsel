package icore

import chisel3._ 
import chisel3.util._ 
import conf.Config 

class Backend extends Module with Config {
  val io = IO(new BackendIO)
}