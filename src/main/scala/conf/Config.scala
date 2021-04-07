package conf

import chisel3._

trait Config {
  var len: Int = 32
  var startAddr: String = "hbfc00000"
  var frontendIssueN: Int = 2   // 1 or 2
  // Cache
  var withRealCache: Boolean = false
  // BPU
  var withBPU: Boolean = false 
  var BPUEntryN: Int = 512
  var BPUOffset: Int = 2
}