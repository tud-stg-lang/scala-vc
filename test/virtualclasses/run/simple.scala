

class Foo  {
  trait Bar  { val x : Int }

  class BarImp extends Bar { val x = 12 }

  trait Bar2 {
    def factory = null
  }

  class X <: { }

  class Y <: { }



  def factory1 = new X
  def factory2 = new Y
  val x = new X()
  var y = new Y()
}

object Foo {
  def main(args: Array[String]) {

      val foo : Foo = new Foo    
      val x : foo.X = new foo.X
      val y : foo.Y = new foo.Y

      println(x != null)
      println(y != null)
      println("Hello world")
  }
}
// vim: set ts=4 sw=4 et:
