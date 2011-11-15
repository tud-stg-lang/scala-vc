
class X { 
  val x = 5


  class Y <: {
    
  }

  class Z(x : Int, y : Int) <: {
      
  }

  def factory1 = new Y
  def factory2(x : Int, y : Int) = new Z(x, y)

  val z = new Z(1,2)
  
}

trait X$trait {
  val x = 5
}


class Xtransformed extends X$trait { }

object X extends App {
  println("works")
}


// vim: set ts=4 sw=4 et:
