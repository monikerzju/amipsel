package conf

import chisel3._

trait Config {
  val len: Int = 32
  val startAddr: String = "hbfc00000"
  val frontendIssueN: Int = 2   // 1 or 2
  val withRealCache: Boolean = false
}