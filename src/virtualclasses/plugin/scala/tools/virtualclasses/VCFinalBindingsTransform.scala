package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform._

/**

 */

abstract class VCFinalBindingsTransform (val global: Global) extends PluginComponent with Transform
with TypingTransformers with InfoTransform with Commons {
  import global._

  override val phaseName = "vc_finalbindings"

  override def transformInfo(sym : Symbol, tpe : Type) =
     infoTransformer(tpe)

  private object infoTransformer extends TypeMap {
    def apply(tpe : Type) = {
      val tpe0 = mapOver(tpe)
      tpe0 match {
        case ClassInfoType(parents, decls0, clazz)
          if (containsInitialBindings(clazz)) =>

          val decls = new Scope
          def enter(sym: Symbol) =
            /*atPhase(ownPhase.next) {*/ decls.enter(sym) /*}*/
          for (m <- decls0) {
            enter(m)
            if (isInitialBinding(m))
              enter(mkFinalBindingSym(m))
          }

          ClassInfoType(parents map this, decls, clazz)

        case x => x
      }
    }
  }

  def mkFinalBindingSym(initBinding : Symbol) : Symbol = {
    val fbname = finalBindingName(initBinding)
    val fbsym = initBinding.owner.enclClass.newClass(fbname, initBinding.pos).setFlag(SYNTHETIC)
    val parents = List(initBinding.tpe, definitions.ScalaObjectClass.tpe)
    val scope = new Scope
    fbsym setInfo ClassInfoType(parents, scope, fbsym)

    //for every abstract type, create alias and provide factory definition
    for (abstpe <- initBinding.info.decls.filter(isVCAbstractType)) {
       //alias type
       val absTpeBinding = abstpe.cloneSymbol(fbsym)
       val workerTrait = initBinding.info.decl(workerTraitName(abstpe))
       absTpeBinding setInfo workerTrait.tpe.substThis(initBinding, ThisType(fbsym))
       absTpeBinding.resetFlag(DEFERRED)
       scope enter absTpeBinding

       //factory
       val factory = initBinding.info.decl(factoryName(abstpe)).cloneSymbol(fbsym)
       factory.resetFlag(DEFERRED)
       factory setInfo factory.tpe.substThis(initBinding, ThisType(fbsym))
       scope enter factory
    }

    fbsym
  }

  def newTransformer(unit : CompilationUnit) = new Transformer(unit)

  class Transformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    protected def finalBindingSym(vc : Symbol) : Symbol = {
      atPhase(ownPhase.next) {
        vc.owner.enclClass.info.member(finalBindingName(vc))
      }
    }

    protected def mkConcreteClassSym(factory: Symbol, initBinding: Symbol) = {
      val workerTraitSym = factory.enclClass.info.member(workerTraitName(factory))
      val cclazz = factory.newClass(factory.pos, concreteClassName(factory))
        .setFlag(SYNTHETIC)
        .setAnnotations(initBinding.annotations)
      val parentClass = (workerTraitSym.info.baseClasses.dropWhile (_.isTrait)).head.tpe
      val mixins = (workerTraitSym.info.baseClasses.filter (_.isTrait)).distinct.reverse.map (_.tpe)
      val parents = (parentClass :: mixins).map(_.substThis(initBinding, ThisType(factory.enclClass)).substSym(workerTraitSym.typeParams, factory.typeParams)) //TODO is this sufficient?

      cclazz setInfo ClassInfoType(parents, new Scope, cclazz)
      cclazz
    }

    protected def mkFactoryDefDef(factory : Symbol, initBinding: Symbol): Tree = {
      val cclazzSym = mkConcreteClassSym(factory, initBinding)
      val cclazzDef = ClassDef(cclazzSym, Modifiers(0), List(List()), List(List()), List(), factory.enclClass.pos.focus)

      val args = factory.paramss map (_.map(Ident))  //TODO clone or not?
      val body = Block(List(cclazzDef),
          TypeApply(Select(New(TypeTree(cclazzSym.tpe.substSym(cclazzSym.typeParams, factory.typeParams)), args), definitions.Any_asInstanceOf),
            List(TypeTree(factory.info.resultType)) )
        )

      atPos(factory.enclClass.pos) {
        DefDef(factory, Modifiers(factory.flags), body)
      }
    }

    protected def mkFinalBinding(initBinding : Symbol) : Tree = {
      val finalBinding = finalBindingSym(initBinding)
      val tpeBindings = finalBinding.info.members.toList.filter(isVCAbstractType) //TODO should we distinguish between abstract type and its concrete binding? If so, use another predicate
      val factories = finalBinding.info.members.toList.filter(_.name.startsWith(FACTORYPREFIX))

      def mkAbsTpeBinding(tpeSym : Symbol) : Tree = {
        val workerTrait = finalBinding.info.member(workerTraitName(tpeSym))
        TypeDef(tpeSym, TypeTree(workerTrait.tpe.substThis(initBinding, ThisType(finalBinding))))
      }

      val body : List[Tree] = (tpeBindings map mkAbsTpeBinding) ::: (factories map (mkFactoryDefDef(_, initBinding)))

      val classDef = ClassDef(finalBinding, Modifiers(0), List(List()), List(List()), body, initBinding.pos)  //TODO which modifiers?

      localTyper.typed {
        atPos(initBinding.pos) {
            classDef
        }
      }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      stats flatMap transformStat
    }

    protected def transformStat(tree: Tree): List[Tree] = {
      val sym = tree.symbol
      tree match {
        case cd @ ClassDef(mods, name, tparams, templ)
          if (isInitialBinding(sym)) =>
          val fbtree = mkFinalBinding(sym)
          //List(ClassDef(mods, name, tparams, transform(templ).asInstanceOf[Template]), fbtree)
          List(cd, fbtree)
        case _ => List(transform(tree))
      }
    }

    //TODO replace ctor calls to initial bindings
    override def transform(tree0 : Tree) : Tree = {
      val tree = super.transform(tree0)

      tree setType atPhase(ownPhase) {
        infoTransformer(tree.tpe)
      }
    }
  }



}
