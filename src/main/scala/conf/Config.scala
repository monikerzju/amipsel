package conf

import chisel3._

trait Config {
  // Compile
  var useLookupBi: Boolean = false
  var withBigCore: Boolean = true   // big core for final
  // Basic Option
  var len: Int = 32
  var startAddr: String = if (withBigCore) "h80000000" else "hbfc00000"
  var endAddr: String = "hbfc00100"
  var trapAddr: String = "hbfc00380"
  var statusVal: String = if (withBigCore)  "b00000000000000000000000000000000" else "b00000000010000000000000000000000"
  // Super Scalar
  var frontendIssueN: Int = 2   // 1 or 2
  var backendIssueN: Int = 2    // 2 backend issue num only affect issue stage
  var backendFuN: Int = 3
  val queueSize: Int = 8
  // Cache
  val traceCache: Boolean = false
  var simpleNBDCache: Boolean = true
  var iTagBits: Int = 21
  var iIndexBits: Int = 6   // 2KB now
  var dTagBits: Int = 21
  var dIndexBits: Int = 6   // 2KB now
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