package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform._


/**
 *  Inserts synthetic factory methods for virtual class member types
 *  and replaces 'new' calls with appropriate factory invocations.
 */
abstract class FactoryTransform(val global: Global) extends PluginComponent with Transform
  with TypingTransformers with InfoTransform {

  import global._

  val FACTORYPREFIX = "new$"

  override val phaseName = "virtualclasses_factories"

  override def transformInfo(sym : Symbol, tpe : Type) = infoTransformer(tpe)

  //TODO this is here for testing purposes
  //removes DEFERRED flags from virtuals and
  //should be moved to a more appropriate phase later on
  private object infoTransformer extends TypeMap {
    def apply(tpe : Type) = mapOver(tpe) match {
      case cit @ ClassInfoType(parents, decls, clazz)
        if(clazz.isVirtualClass) =>
          clazz.resetFlag(DEFERRED)
          cit

      case x => x

    }
  }


  def newTransformer(unit: CompilationUnit) = new FactoryTransformer(unit)

  class FactoryTransformer(val unit: CompilationUnit) extends TypingTransformer(unit) {

    protected def mkFactorySym(owner: Symbol, clazz: Symbol): Symbol = {
      val factorySym = owner.newMethod(clazz.pos, factoryName(clazz)
      val tparams =  clazz.typeParams map (_.cloneSymbol(factorySym))

      def cloneAndSubst(s : Symbol) : Symbol = {
        s.cloneSymbol(factorySym).setInfo(
        s.tpe.substSym(clazz.typeParams, tparams))
      }
      val params = clazz.primaryConstructor.paramss flatMap  ( _.map (cloneAndSubst))
      //val tparams = clazz.typeParams map ( s => { val skolem = s.cloneSymbol(factorySym).newTypeSkolem
                                                  //skolem.setInfo(s.info.cloneInfo(skolem))
                                                 // skolem})
      factorySym.setInfo(polyType(tparams, MethodType(params, clazz.tpe.substSym(clazz.typeParams, tparams))))
      factorySym.setFlag(clazz.flags & AccessFlags | SYNTHETIC)
      factorySym
    }

    protected def containsVirtualClasses(sym: Symbol) = sym.info.decls exists (_.isVirtualClass)
    protected def getVirtualClasses(sym: Symbol) = sym.info.decls.toList filter (_.isVirtualClass)

    /** Names of derived classes and factories */
    protected def concreteClassName(clazz: Symbol) =
      newTypeName(clazz.name + "$fix")
    protected def factoryName(clazz: Symbol) =
      newTermName(FACTORYPREFIX + clazz.name)

    //TODO support for overloaded constructors
    //TODO annotations
    protected def mkFactoryDefDef(owner: Symbol, clazz: Symbol): Tree = {
      val factorySym = mkFactorySym(owner, clazz)
      val args = factorySym.paramss map (_.map(Ident))  //TODO clone or not?
      val ctorCall = New(TypeTree(clazz.tpe.substSym(clazz.typeParams, factorySym.typeParams)), args)

      localTyper.typed {
        atPos(clazz.pos) {
            DefDef(factorySym, Modifiers(factorySym.flags), ctorCall)
        }
      }
    }

    protected def isFactoryDefDef(defdef : DefDef) = {
      defdef.symbol.hasFlag(SYNTHETIC) &&
      defdef.rhs.symbol.owner.isVirtualClass &&
      defdef.symbol.name.startsWith(FACTORYPREFIX) &&
      defdef.symbol.name.endsWith(defdef.rhs.symbol.owner.name) &&
      defdef.symbol.name.length == (FACTORYPREFIX.length + defdef.rhs.symbol.owner.name.length)
    }

    override def transform(tree : Tree) : Tree = {
      postTransform(preTransform(tree))
    }

    protected def postTransform(tree : Tree) : Tree = {
      tree match {
        case defdef: DefDef if(isFactoryDefDef(defdef)) => defdef

        case app @ Apply(sel @ Select(New(tpt), nme.CONSTRUCTOR), args)
           if(app.symbol.owner.isVirtualClass && app.symbol.isConstructor) =>

           val clazz = app.symbol.owner

           localTyper.typed {
             atPos(tree.pos) {
               gen.mkMethodCall(Select(gen.mkAttributedQualifier(tpt.tpe.prefix),
                                       factoryName(clazz)),
                                tpt.tpe.typeArgs,
                                args)
             }
           }

        case _ => super.transform(tree)
      }
    }

    protected def preTransform(tree: Tree): Tree = {
      tree match {

        case classdef: ClassDef if(containsVirtualClasses(classdef.symbol)) =>
            val synthesized = getVirtualClasses(classdef.symbol) map (mkFactoryDefDef (classdef.symbol,_))

            // add the synthesized methods
            var template = classdef.impl
            template = treeCopy.Template(template, template.parents,
              template.self, synthesized ::: template.body)

            // switch the implementation
            val result = localTyper.typed(treeCopy.ClassDef(classdef, classdef.mods, classdef.name,
              classdef.tparams, template))

            //to make localTyper happy in postTransform
            for(facSym <- synthesized map  (_.symbol))
              result.symbol.info.decls.enter(facSym)

            result

        case _ => tree
      }
    }


  }
}
