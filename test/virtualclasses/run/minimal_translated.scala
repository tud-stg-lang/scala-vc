/*class X {

  class Y <: {
    val bla : Y = new Y()
  }

}*/

class X {
  type Y <: VC_TRAIT$Y

  trait VC_TRAIT$Y { self: Y =>
    val bla : Y = VC_NEW$Y()
  }
  
  def VC_NEW$Y() : Y
}

class VC_FINAL$X extends X {
  type Y = VC_TRAIT$Y
  
  class VC_FIX$Y extends VC_TRAIT$Y {
  
  }
  
  def VC_NEW$Y(): Y = new VC_FIX$Y()
}