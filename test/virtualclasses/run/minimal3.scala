object minimal {
  
  def main(args: Array[String]): Unit = {
    val a: A = new VC_FINAL_T$A()
    val b: a.B = a.VC_NEW_T$B()
  }
}

class X {

  class Y <: {
    val bla : Y = null
    val foo = "Bar"
  }

}

abstract class A {
  type B >: Null <: VC_TRAIT_T$B

  trait VC_TRAIT_T$B { self: B =>
    //
    val bla : B = null
  }
  
  def VC_NEW_T$B() : B
}

class VC_FINAL_T$A extends A {
  type B = VC_TRAIT_T$B
  
  class VC_FIX_T$B extends VC_TRAIT_T$B {
  }
  
  def VC_NEW_T$B(): B = {
    new VC_FIX_T$B()
  }
}