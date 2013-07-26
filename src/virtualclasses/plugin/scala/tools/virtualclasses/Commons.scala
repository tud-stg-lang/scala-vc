package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.transform._

/**

  */
trait Commons {

  val global : Global

  import global._


  //Prefixes for synthesized symbol names:
  //factories
  val FACTORYPREFIX = "VC_NEW$"
  //workertraits
  val TRAITPREFIX = "VC_TRAIT$"
  //workertrait implementations
  val CCLASSPREFIX = "VC_FIX$"
  //final bindings
  val FINALPREFIX = "VC_FINAL$"
  //this is here for debugging purposes, the abstract types will inherit the virtual class' name eventually
  val ABSTPEPREFIX = "VC_T$"
  //dito for initial bindings
  val INITPREFIX = "VC_INIT$"

  def isVCAbstractType(sym : Symbol) : Boolean = {
    //TODO find an alternative way to determine this (add new flags?)
    sym.name.startsWith(ABSTPEPREFIX)
  }

  def finalBindingName(sym : Symbol) : TypeName = {
    if (sym.name.startsWith(FINALPREFIX))
      return sym.name.toTypeName

    val prefixToStrip =
      if(sym.name.startsWith(INITPREFIX))
        INITPREFIX
      else ""

    newTypeName(FINALPREFIX+(sym.name.toString().stripPrefix(prefixToStrip)))
  }

  def isInitialBinding(sym : Symbol) : Boolean = {
    //TODO find an alternative way to determine this (add new flags?)
    sym.isAbstractClass && sym.info.members.exists(x => x.isTrait && x.name.startsWith(TRAITPREFIX))
  }

  def containsInitialBindings(sym : Symbol) : Boolean = {
    sym.info.members.exists(isInitialBinding)
  }

  def stripAllPrefixes(name: Name) = {
    val prefixToStrip =
      if(parent.startsWith(ABSTPEPREFIX))
        ABSTPEPREFIX
      else if (parent.startsWith(FACTORYPREFIX))
        FACTORYPREFIX
      else if (parent.startsWith(CCLASSPREFIX))
        CCLASSPREFIX
      else if (parent.startsWith(FINALPREFIX))
        FINALPREFIX
      else ""
    newTypeName(name.toString.stripPrefix(prefixToStrip))
  }

  def factoryName(clazz: Symbol) = { //TODO make method more robust
    val prefixToStrip =
    if(clazz.name.startsWith(ABSTPEPREFIX))
      ABSTPEPREFIX
    else if (clazz.name.startsWith(TRAITPREFIX))
      TRAITPREFIX
    else if (clazz.name.startsWith(CCLASSPREFIX))
      CCLASSPREFIX
    else ""

    newTermName(FACTORYPREFIX+(clazz.name.toString().stripPrefix(prefixToStrip)))
  }

  def workerTraitName(sym: Symbol) : TypeName =
    workerTraitName(sym.name, sym.owner.name)

  def workerTraitName(name: Name, parent: Name) : TypeName = {
    if(name.startsWith(TRAITPREFIX))
      return name.toTypeName

    val prefixToStrip =
      if(name.startsWith(ABSTPEPREFIX))
        ABSTPEPREFIX
      else if (name.startsWith(FACTORYPREFIX))
        FACTORYPREFIX
      else if (name.startsWith(CCLASSPREFIX))
        CCLASSPREFIX
      else ""

    val parentprefixToStrip =
      if(parent.startsWith(ABSTPEPREFIX))
        ABSTPEPREFIX
      else if (parent.startsWith(FACTORYPREFIX))
        FACTORYPREFIX
      else if (parent.startsWith(CCLASSPREFIX))
        CCLASSPREFIX
      else if (parent.startsWith(FINALPREFIX))
        FINALPREFIX
      else ""

    newTypeName(TRAITPREFIX + parent.toString.stripPrefix(parentprefixToStrip) + "$" +(name.toString().stripPrefix(prefixToStrip)))
  }

  def concreteClassName(sym : Symbol) : TypeName = {
    if (sym.name.startsWith(TRAITPREFIX))
      return sym.name.toTypeName

    val prefixToStrip =
      if(sym.name.startsWith(ABSTPEPREFIX))
        ABSTPEPREFIX
      else if (sym.name.startsWith(FACTORYPREFIX))
        FACTORYPREFIX
      else if (sym.name.startsWith(TRAITPREFIX))
        TRAITPREFIX
      else ""

    newTypeName(CCLASSPREFIX+(sym.name.toString().stripPrefix(prefixToStrip)))
  }
}