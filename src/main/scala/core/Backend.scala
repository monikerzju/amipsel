package core

class Backend extends Module with Config {
  val io = IO(new BackendIO)
}