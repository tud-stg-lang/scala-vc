class F {

   class Y <: {
     def y : Y 
     val x = 1
     def foo = ""
   }

   var y : Y = null
}


class FTransformed {

  type Y <: trait$Y

  trait trait$Y { //self : Y with trait$Y =>
    def y : Y 
    val x = 1
    def foo = ""
  }

  def new$Y : Y = {
    class imp$Y extends trait$Y {
      //self : Y with trait$Y with imp$Y =>
    }

    //(new imp$Y).asInstanceOf[Y]
    (null).asInstanceOf[Y]
  }

  var y : Y = null.asInstanceOf[Y]

}

object F extends App {
  println("compiles")
}
