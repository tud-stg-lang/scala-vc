class X {

  class Y <: {
    val bla : Y = new Y()
    val foo = "Bar"
  }

}


abstract class A {
  type B <: VC_TRAIT_T$B

  trait VC_TRAIT_T$B { self: B =>
    //
    val bla : B = VC_NEW_T$B()
  }
  
  def VC_NEW_T$B() : B
}

class VC_FINAL_T$A extends A {
  type B = VC_TRAIT_T$B
  
  class VC_FIX_T$B extends VC_TRAIT_T$B {
  
  }
  
  def VC_NEW_T$B(): B = new VC_FIX_T$B()
}

object F {
  def main(args: Array[String]) {
      //val x = new VC_FINAL$X()
      //val y = x.VC_NEW$Y()
      
      println("Something works!")
    }
}

// vim: set ts=4 sw=4 et:
