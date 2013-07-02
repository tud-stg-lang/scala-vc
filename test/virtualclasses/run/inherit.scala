class Foo  {
  class X <: { val x = 10 }

  class Y <: { }



  def factory1 = new X
  def factory2 = new Y
  val x = new X()
  var y = new Y()
}

class Bar extends Foo {
	class X <: { val y = 20 }
}

object Foo {
  def main(args: Array[String]) {

      val foo : Foo = new Foo    
      val x : foo.X = new foo.X
      val y : foo.Y = new foo.Y

      println(x != null)
      println(y != null)
      println("Hello world")
      println(x.x)
      
      val bar : Bar = new Bar
      val bx : bar.X = new bar.X
      //println(bx.x)
      //println(bx.y)
  }
}