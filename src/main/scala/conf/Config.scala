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
  val traceCache: Boolean = true
  var simpleNBDCache: Boolean = true
  var iTagBits: Int = 19
  var iIndexBits: Int = 8   // 8KB now
  var dTagBits: Int = 18
  var dIndexBits: Int = 9   // 16KB now
  var offsetBits: Int = 5
  var dataBits: Int = 256
  // BPU
  var traceCallRet: Boolean = false
  var traceBPU: Boolean = false
  var BPUEntryN: Int = 256
  var BPUOffset: Int = 32  // 2 is same with 3
  val BHTCacheEntryN: Int = 1
  // Assertions ---- Dont Change!!!
  assert(len == 32)
  assert(frontendIssueN == 1 || frontendIssueN == 2)
  assert(backendIssueN == 2)
  assert(iTagBits + iIndexBits + offsetBits == len)
  assert(dTagBits + dIndexBits + offsetBits == len)
  assert(backendFuN == 3)
  assert(BPUOffset >= 2 && BPUOffset <= 5 || BPUOffset == 32)
  assert(BPUEntryN == 128 || BPUEntryN == 256)
  assert(BHTCacheEntryN == 1)
}