package conf

import chisel3._

trait Config {
  // Compile
  var useLookupBi: Boolean = false
  // Basic Option
  var len: Int = 32
  var startAddr: String = "hbfc00000"
  var endAddr: String = "hbfc00100"
  var trapAddr: String = "hbfc00380"
  var statusVal: String = "b00000000010000000000000000000000"
  var withBigCore: Boolean = false   // big core for final
  // Super Scalar
  var frontendIssueN: Int = 2   // 1 or 2
  var backendIssueN: Int = 2    // 2 backend issue num only affect issue stage
  var backendFuN: Int = 3
  val queueSize: Int = 8
  // Cache
  var dcacheMetaZeroLatency: Boolean = false
  // var icachePref: Boolean = false
  var tagBits: Int = 19
  var indexBits: Int = 8   // 8KB now
  var offsetBits: Int = 5
  var dataBits: Int = 256
  // BPU
  var withBPU: Boolean = false 
  var BPUEntryN: Int = 512
  var BPUoffset: Int = 2
  var enableRAS: Boolean = true
  val withRAS: Boolean = enableRAS && withBPU
  var RASEntryN: Int = 6
  var withBS: Boolean = false 
  var BSEntryN: Int = 3
  var BSWayN: Int = 2
  // Assertions ---- Dont Change!!!
  assert(len == 32)
  assert(frontendIssueN == 1 || frontendIssueN == 2)
  assert(backendIssueN == 2 || backendIssueN == 3)
  assert(tagBits + indexBits + offsetBits == len)
  assert(backendFuN == 3)
}