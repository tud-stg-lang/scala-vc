package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers

/**
 *  Inserts synthetic factory methods for virtual class member types
 *  and replaces 'new' calls with appropriate factory invocations.
 *
 *  This class implements a plugin component using tree transformers and
 *  InfoTransformer. An InfoTransformer will be automatically created
 *  and registered in <code>SymbolTable.infoTransformers</code>. If
 *  a <code>Typer</code> is needed during transformation, the component
 *  should mix in <code>TypingTransformers</code>. This provides a local
 *  variable <code>localTyper: Typer</code> that is always updated to
 *  the current context.
 */
class FactoryTransform(val global: Global) extends PluginComponent
  with TypingTransformers
  with InfoTransform {

  import global._
  import global.definitions._

  val runsBefore = List[String]("refchecks")
  val phaseName = "virtualclasses_factories"

  def transformInfo(sym: Symbol, tp: Type): Type = infoTransformer.mapOver(tp)

  def newTransformer(unit: CompilationUnit) = new NewTransformer(unit)

  /**
   * The type transformation applied by this component. The trait InfoTransform
   *  will create an instance of InfoTransformer applying this TypeMap. The type
   *  map will be applied when computing a symbol's type in all phases
   *  <em>after</em> "virtualclasses"
   */
  private val infoTransformer = new TypeMap {
    def apply(tp: Type): Type = mapOver(tp) match {
      case ClassInfoType(parents, decls0, clazz) 
        if (clazz.isVirtualClass) =>
          println("infoTransformer: virtual class " + clazz)
          val decls = new Scope(decls0)
          for (member <- decls0; if member.isClass && !member.isAbstract)
            decls.enter(mkFactory(member, clazz))

          ClassInfoType(parents, decls, clazz)

      case tp0 => tp0
    }
  }

  /*
   * Creates a synthetic factory method symbol for
   * a given member class in an enclosing virtual class.
   * The factory has the same AccessFlags as the
   * member class.
   *
   * @param memClaz The member class
   * @param clazz The enclosing virtual class
   */
  protected def mkFactory(memClazz: Symbol, clazz: Symbol) = {
    println("creating factory symbol for " + memClazz + " in class " + clazz)

    val factory = clazz.newMethod(clazz.pos, factoryName(clazz))
      .setFlag(memClazz.flags & AccessFlags | SYNTHETIC)
      .setAnnotations(memClazz.annotations)

    factory setInfo new PolyTypeCompleter(factory, memClazz) {
      private def copyType(tpe: Type): Type = tpe match {
        case MethodType(formals, restpe) => MethodType(formals, copyType(restpe))
        case NullaryMethodType(restpe) => NullaryMethodType(copyType(restpe))
        case PolyType(_, _) => abort("bad case: " + tpe)
        case _ => clazz.thisType.memberType(memClazz)
      }
      def getInfo = copyType(memClazz.primaryConstructor.tpe)
    }

    factory
  }

  /** Names of derived classes and factories */
  protected def concreteClassName(clazz: Symbol) =
    atPhase(ownPhase) { newTypeName(clazz.name + "$fix") }
  protected def factoryName(clazz: Symbol) =
    atPhase(ownPhase) { newTermName("new$" + clazz.name) }

  /**
   * A lazy type to complete `sym`, which is is generated for virtual class
   *  `clazz`.
   *  The info of the symbol is computed by method `getInfo`.
   *  It is wrapped in copies of the type parameters of `clazz`.
   */
  abstract class PolyTypeCompleter(sym: Symbol, clazz: Symbol) extends LazyType {
    def getInfo: Type
    override val typeParams = cloneSymbols(clazz.typeParams, sym)
    override def complete(sym: Symbol) {
      sym.setInfo(
        getInfo.substSym(clazz.typeParams, typeParams))
    }
  }

  class NewTransformer(val unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree0: Tree): Tree = {
      val tree = super.transform(tree0)
      tree match {
	case cdef @ ClassDef(mods, name, tparams, impl)
          if (cdef.symbol.isVirtualClass && !cdef.symbol.isAbstract) => 
            println("inserting factory method into the ast for member class " 
		    + cdef.symbol + " in virtual class " + cdef.symbol.owner)
  
            addFactoryDef(mods, name, tparams, impl)  

        case app @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args)
          if (isVCMemberClass(app.symbol) && app.symbol.isConstructor) =>
	    println("replacing constructor call for type " + app.tpe  + " at pos " + app.pos) 

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
            }

        case _ => tree
      }
    }

    protected def mkFactoryCall(tpe: Type, args: List[Tree]) = throw new Exception("not implemented")
    protected def addFactoryDef(mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) = throw new Exception("not implemented")
    protected def isVCMemberClass(sym : Symbol) = sym.isClass && sym.owner.isVirtualClass

  }
}
