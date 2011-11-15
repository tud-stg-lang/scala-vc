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
  with TypingTransformers {

  import global._


  override val phaseName = "virtualclasses_factories"

  def mkFactorySym(owner: Symbol, clazz: Symbol): Symbol = {
      val argSyms = clazz.primaryConstructor.info.params
      val factorySym = owner.newMethod(clazz.pos, factoryName(clazz))
      factorySym.setInfo(MethodType(argSyms, clazz.tpe))
      factorySym.setFlag(SYNTHETIC)

      factorySym
  }

  protected[FactoryTransform] def containsVirtualClasses(sym: Symbol) = sym.info.decls exists (_.isVirtualClass)
  protected[FactoryTransform] def getVirtualClasses(sym: Symbol) = sym.info.decls.toList filter (_.isVirtualClass)

  /** Names of derived classes and factories */
  protected[FactoryTransform] def concreteClassName(clazz: Symbol) =
     newTypeName(clazz.name + "$fix")
  protected[FactoryTransform] def factoryName(clazz: Symbol) =
     newTermName("new$" + clazz.name)

  def newTransformer(unit: CompilationUnit) = new FactoryTransformer(unit)

  class FactoryTransformer(val unit: CompilationUnit) extends TypingTransformer(unit) {

    //TODO support for overloaded constructors
    //TODO factory needs to inherit (some) flags from class
    //TODO annotations
    //TODO type parameters for factory
    def mkFactoryDefDef(owner: Symbol, clazz: Symbol): Tree = {
      val sym = mkFactorySym(owner, clazz)
      val args = sym.paramss map (_.map(Ident))
      val ctorCall = New(TypeTree(clazz.tpe), args)

      localTyper.typed {
        atPos(clazz.pos) {
            DefDef(sym, Modifiers(sym.flags), ctorCall)
        }
      }
    }


    override def transform(tree : Tree) : Tree = {
      postTransform(preTransform(tree))
    }

    protected def postTransform(tree0 : Tree) : Tree = {
      val tree = super.transform(tree0)
      tree match {
        case  app @ Apply(sel @ Select(New(tpt), nme.CONSTRUCTOR), args)
           if(app.symbol.owner.isVirtualClass && app.symbol.isConstructor) =>

           val clazz = app.symbol.owner
           val factorySelect = Select(TypeTree(clazz.owner.tpe),
                                      factoryName(clazz))
           val targs = tpt.tpe.typeArgs

         localTyper.typed {
             atPos(tree.pos) {
                 Apply(if (targs.isEmpty)
                          factorySelect
                       else TypeApply(factorySelect, targs map TypeTree),
                       args)
             }
         }

        case _ => tree
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
