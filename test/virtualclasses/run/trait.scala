
class X { 
  val x = 5


  class Y <: {
    
  }

  class Z(x : Int, y : Int) <: {
     def this(s : String) {
       this(1,2)
     }

     def this(x : Int) {
       this(1,2)
     }
  }

  def factory1 = new Y
  def factory2(x : Int, y : Int) = new Z(x, y)
  def factory3 = factory2(3,4)

  val z = new Z(1,2)
  
}

class T(val o : X) { 

val z = new o.Z(2,3)
  val y = new o.Y()
  val y1 = o.factory1
  val z1 = o.factory2(2,3)

}


class Foo {

  class Bar[T1,T2] <: {
      def factory1 = new Bar[T1,T2]
      def factory2 = new Bar[Int,String]
  }

}

object X extends App {
  println("compiles")

  val t = new T(new X)

  println(t.y)
  println(t.z)
  println(t.y1)
  println(t.z1)

}


// vim: set ts=4 sw=4 et:
