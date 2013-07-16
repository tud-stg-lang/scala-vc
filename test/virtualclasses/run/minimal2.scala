class X {

  class Y <: {
    val bla : Y = null
    val foo = "Bar"
  }

}

object F {
  def main(args: Array[String]) {
      //val x = new VC_FINAL$X()
      //val y = x.VC_NEW$Y()
      
      println("Something works!")
    }
}

// vim: set ts=4 sw=4 et:
