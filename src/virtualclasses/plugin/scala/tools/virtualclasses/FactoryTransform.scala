package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.InfoTransform

/**
 *  Inserts synthetic factory methods for virtual class member types
 *  and replaces 'new' calls with appropriate factory invocations.
 */
abstract class FactoryTransform(val global: Global) extends PluginComponent
  with TypingTransformers
  with InfoTransform {

  import global._
  import global.definitions._

  override val runsBefore = List[String]("refchecks") //TODO before superaccessors??
  override val phaseName = "virtualclasses_factories"

  def transformInfo(sym: Symbol, tpe: Type) = tpe

  def newTransformer(unit: CompilationUnit) = new FactoryTransformer(unit)

  /** Names of derived classes and factories */
  protected def concreteClassName(clazz: Symbol) =
    atPhase(ownPhase) { newTypeName(clazz.name + "$fix") }
  protected def factoryName(clazz: Symbol) =
    atPhase(ownPhase) { newTermName("new$" + clazz.name) }

  class FactoryTransformer(val unit: CompilationUnit) extends TypingTransformer(unit) {

    def factoryDef(owner: Symbol, clazz: Symbol): Tree = {
      //      var mods = Modifiers(SYNTHETIC)
      val sym = owner.newMethod(clazz.pos, factoryName(clazz))
      sym.setInfo(MethodType(List(), clazz.tpe))
      sym.setFlag(SYNTHETIC)

      localTyper.typed {
        atPos(clazz.pos) {
          DefDef(sym, Literal(Constant(null)))
          /*DefDef(mods, factoryName(clazz),
                List(), List(), TypeTree(clazz.tpe), Literal(Constant(null))) */
        }
      }
    }

    /** The factory corresponding to a virtual class. */
    protected def factory(clazz: Symbol, owner: Symbol) = atPhase(ownPhase.next) {
      val fsym = owner.info.member(factoryName(clazz))
      assert(fsym.isMethod, clazz)
      fsym
    }

    override def transform(tree0: Tree): Tree = {
      val tree = super.transform(tree0)
      tree match {

        case classdef: ClassDef if(containsVirtualClasses(classdef)) =>
            val synthesized = getVirtualClasses(classdef) map (factoryDef (classdef.symbol,_))

            // add the synthesized methods
            var template = classdef.impl
            template = treeCopy.Template(template, template.parents,
              template.self, synthesized ::: template.body)

            // switch the implementation
            val result = treeCopy.ClassDef(classdef, classdef.mods, classdef.name,
              classdef.tparams, template)

            result

        case app @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args) if (isVCMemberClass(app.symbol) && app.symbol.isConstructor) =>
          /* println("replacing constructor call for type " + app.tpe  + " at pos " + app.pos) 

	    val clazz = app.symbol.owner
            val fn = 
            Select(
              gen.mkAttributedQualifier(tpt.tpe.prefix),
              factory(clazz, clazz.owner).name)
            println("fac "+factory(clazz, clazz.owner).tpe)
            val targs = tpt.tpe.typeArgs
            atPos(tree.pos) {
		localTyper.typed {
		  val res = 
			Apply(if (targs.isEmpty) fn else TypeApply(fn, targs map TypeTree), args)
		  println("typing "+res+" from "+args)
                  res
		}
            } */
          tree


        case _ => tree
      }
    }

    protected def mkFactoryCall(tpe: Type, args: List[Tree]) = throw new Exception("not implemented")
    protected def addFactoryDef(mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template): Tree = throw new Exception("not implemented")
    protected def isVCMemberClass(sym: Symbol) = sym.isClass && sym.owner.isVirtualClass

    protected def containsVirtualClasses(tree: ClassDef) = tree.symbol.info.decls exists (_.isVirtualClass)
    protected def getVirtualClasses(tree: ClassDef) = tree.symbol.info.decls.toList filter (_.isVirtualClass)
  }
}
