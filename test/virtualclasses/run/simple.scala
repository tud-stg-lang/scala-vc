

class Foo <: {
  trait Bar  { val x : Int }

  class BarImp extends Bar { val x = 12 }

  trait Bar2 {
    def factory = null
  }

  class X <: { }

  class Y <: { }



  def factory1 = new X
  def factory2 = new Y
}

object Foo {
  def main(args: Array[String]) {
      println("Hello world")
  }
}
// vim: set ts=4 sw=4 et:
