
abstract class X { 
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

class XImp extends X {

  

}

object X extends App {
  println("works")
}


// vim: set ts=4 sw=4 et:
