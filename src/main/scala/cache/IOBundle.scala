package cache
import conf._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import icore._

trait Cache_Parameters{
    val TagBits=18
    val IndexBits=8
    val OffsetBits=6
    val DataBits=512
    val nBuf=4
    // FIXME: not compatible with current interface
    assert(TagBits+IndexBits+OffsetBits==32)
    val MissTolerance=4
}
