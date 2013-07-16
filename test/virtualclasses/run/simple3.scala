class Foo  {
  class X <: { val truth = 42 }

  class Y <: { }



  def factory1 = new X
  def factory2 = new Y
  val x = new X()
  var y = new Y()
}

class Bar extends Foo {
	val z = 5
}

object Foo {
  def main(args: Array[String]) {

      val foo : Foo = new Foo    
      val x : foo.X = new foo.X
      val y : foo.Y = new foo.Y

      println(x.truth)
      println(y != null)
      println("Hello world")
      println(foo.x.truth)
      
      val bar : Bar = new Bar
      val bx : bar.X = new bar.X
      println(bx.truth)
      println(bar.z)
  }
}
// vim: set ts=4 sw=4 et:
