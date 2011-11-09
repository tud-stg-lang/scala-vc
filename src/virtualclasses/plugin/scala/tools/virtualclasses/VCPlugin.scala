// $Id$

package scala.tools.virtualclasses

import scala.tools.nsc
import scala.tools.nsc.typechecker._
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class VCPlugin(val global: Global) extends Plugin {
  import global._

  val name = "virtualclasses"
  val description = "adds support for virtual classes"
  
  val addFactoriesPhase = new FactoryTransform(this.global)
  
  val components = List[PluginComponent](addFactoriesPhase)

  global.log("instantiated virtualclasses plugin: " + this)

  def setEnabled(flag: Boolean) = {
   // newPhase setEnabled flag
  }

  // TODO: require -enabled command-line flag
  
  override def processOptions(options: List[String], error: String => Unit) = {
    var enabled = false
    for (option <- options) {
      if (option == "enable") {
        enabled = true
      } else {
        error("Option not understood: "+option)
      }
     }
    setEnabled(enabled)
  }

  override val optionsHelp: Option[String] =
    Some("  -P:virtualclasses:enable        Enable virtual classes")
}
