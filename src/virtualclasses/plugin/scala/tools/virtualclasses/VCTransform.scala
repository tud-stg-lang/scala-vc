package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform._


abstract class VCTransform(val global: Global) extends PluginComponent with Transform
  with TypingTransformers with InfoTransform {

  import global._
                  
  val FACTORYPREFIX = "VC_NEW$"
  val TRAITPREFIX = "VC_TRAIT$"


  override val phaseName = "virtualclasses"

  override def transformInfo(sym : Symbol, tpe : Type) =
    if (sym.isThisSym && sym.owner.isVirtualClass) {
        abstractType(sym.owner).typeConstructor //TODO compare with devirtualize
    }
    else infoTransformer(tpe)

  //part of this is from the old DeVirtualize implementation
  private object infoTransformer extends TypeMap {
    def apply(tpe : Type) = {
      val tpe0 = mapOver(tpe)
      tpe0 match {
      case tp1 @ ClassInfoType(parents, decls0, clazz)
        if (containsVirtualClasses(clazz)) =>
        
          val decls = new Scope
          def enter(sym: Symbol) =
            atPhase(ownPhase.next) { decls.enter(sym) }
                   
          for (m <- decls0.toList) {
            if (m.isVirtualClass) {
              //do NOT put original symbol of virtual class back
              val newM = m.cloneSymbol(m.owner)
                  newM.setFlag(TRAIT | SYNTHETIC)
                  newM.resetFlag(DEFERRED)
                  newM.name = workerTraitName(m)
              fixCtors(newM)

              enter(mkAbstractTypeSym(clazz, m, newM.tpe))
              enter(mkFactorySym(clazz, m))
              enter(newM)
            }
            else enter(m)
          }
          ClassInfoType(parents map this, decls, clazz)

      case ClassInfoType(_, _, clazz)
        if (clazz.isVirtualClass) =>
            val workerTrait = atPhase(ownPhase.next) { clazz.owner.info.member(workerTraitName(clazz)) }
            workerTrait.info

      case tp1 @ TypeRef(pre, clazz, args) if clazz.isVirtualClass =>
        typeRef(pre, abstractType(clazz.owner, clazz.name), args)

      case x => x

      }
    }

    /**
     * Remove constructor symbols and add mixin-ctor symbol
     * for newly created worker trait symbol.
     *
     * TODO how to handle multiple ctors?
     * */
    def fixCtors(workerTrait : Symbol) {
      atPhase(ownPhase) {
        val sym = workerTrait.newMethod(workerTrait.pos, nme.MIXIN_CONSTRUCTOR)
        sym setInfo MethodType(List(), definitions.UnitClass.tpe)
        workerTrait.info.decls.enter(sym)

        val ctor = workerTrait.info.member(nme.CONSTRUCTOR)
        workerTrait.info.decls.unlink(ctor)
      }
    }
  }

  /** The abstract type corresponding to a virtual class. */
  protected def abstractType(clazz: Symbol): Symbol =
    abstractType(clazz.owner, atPhase(ownPhase) { clazz.name })

  /** The abstract type corresponding to a virtual class. */
  protected def abstractType(owner: Symbol, name : Name): Symbol = {
    atPhase(ownPhase.next) {
      val abstpe = owner.info.decl(name)
      assert(abstpe.isAbstractType)
      abstpe
    }
  }

  protected def mkAbstractTypeSym(owner : Symbol, clazz : Symbol, upperBound : Type) : Symbol = {
    val absTpe =
      owner.newAbstractType(clazz.pos, clazz.name.asInstanceOf[TypeName])
        .setFlag(clazz.flags & (AccessFlags | DEFERRED) | SYNTHETIC)
        .setAnnotations(clazz.annotations)

   // atPhase(ownPhase.next) {
      absTpe.setInfo(TypeBounds(definitions.NothingClass.tpe, upperBound))
      absTpe
    //}
  }

  protected def mkFactorySym(owner: Symbol, clazz: Symbol): Symbol = {
    atPhase(ownPhase) {
      val factorySym = owner.newMethod(clazz.pos, factoryName(clazz))
      val tparams =  clazz.typeParams map (_.cloneSymbol(factorySym))

      def cloneAndSubst(s : Symbol) : Symbol = {
        s.cloneSymbol(factorySym).setInfo(
          s.tpe.substSym(clazz.typeParams, tparams))
      }
      val params = clazz.primaryConstructor.paramss flatMap  ( _.map (cloneAndSubst))
      factorySym.setInfo(polyType(tparams, MethodType(params, clazz.tpe.substSym(clazz.typeParams, tparams))))
      factorySym.setFlag(clazz.flags & AccessFlags | SYNTHETIC)
      factorySym
    }
  }

  def newTransformer(unit: CompilationUnit) = new VCTransformer(unit)

  protected def containsVirtualClasses(sym: Symbol) = sym.info.decls exists (_.isVirtualClass)
  protected def getVirtualClassSymbols(sym: Symbol) = atPhase(ownPhase) { sym.info.decls.toList filter (_.isVirtualClass) }

  protected def factoryName(clazz: Symbol) = atPhase(ownPhase) {
    newTermName(FACTORYPREFIX+clazz.name)
  }
  protected def factoryName(name: Name) =
    newTermName(FACTORYPREFIX+name)


  protected def workerTraitName(clazz: Symbol) = atPhase(ownPhase) {
    newTypeName(TRAITPREFIX+clazz.name)
  }

  class VCTransformer(val unit: CompilationUnit) extends TypingTransformer(unit) {

    /** The factory symbol corresponding to a virtual class. */
    protected def factory(owner: Symbol, clazz : Symbol) = {
      atPhase(ownPhase.next) {
        val fsym = owner.info.member(factoryName(clazz.name))
        assert(fsym.isMethod, clazz)
        fsym
      }
    }

    /** The mixin constructor symbol of the workertrait for given virtual class */
    protected def mixinCtor(clazz : Symbol) = {
      atPhase(ownPhase.next) {
        val workerTrait = clazz.owner.info.member(workerTraitName(clazz))
        val csym = workerTrait.info.member(nme.MIXIN_CONSTRUCTOR)
        assert(csym.isConstructor, clazz)
        csym
      }
    }

    //TODO at the moment this does not work as expected
    protected def wasVirtualClass(clazz : Symbol) = {
      atPhase(ownPhase) {
        clazz.isVirtualClass
      }
    }

    override def transformUnit(unit: CompilationUnit) {
      atPhase(ownPhase.next) {
        super.transformUnit(unit)
      }
    }

    //TODO support for overloaded constructors
    //TODO annotations
    protected def mkFactoryDefDef(owner: Symbol, clazz: Symbol): Tree = {
      val factorySym = factory(owner, clazz)
      val args = factorySym.paramss map (_.map(Ident))  //TODO clone or not?
      //TODO fix type parameters again
      val body = TypeApply(Select(Literal(Constant(null)), definitions.Any_asInstanceOf),
                                     List(TypeTree(factorySym.info.resultType)) )
      //TODO implement the real body
        
      localTyper.typed {
        atPos(clazz.pos) {
            DefDef(factorySym, Modifiers(factorySym.flags), body)
        }
      }
    }

    protected def mkAbstractTypeDef(clazz : Symbol) : Tree = {
      val typeSym = abstractType(clazz)
      localTyper.typed {
        atPos(clazz.pos) {
          TypeDef (typeSym)
        }
      }
    }

    /** Add trees for abstract types, worker traits, and factories
     *  to template body `stats`
     */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      stats flatMap transformStat
    }

    /** Replace definitions of virtual classes by definitions of corresponding
     *  abstract type and worker traits.
     *  Eliminate constructors of former virtual classes because these are now traits.
     */
    protected def transformStat(tree: Tree): List[Tree] = {
      val sym = tree.symbol
      tree match {
        case ClassDef(mods, name, tparams, templ)
          if (wasVirtualClass(sym)) =>
          val clazz = sym

          val workerTraitSym = atPhase(ownPhase.next) { clazz.owner.info.member(workerTraitName(clazz)) }
          val cdef = ClassDef(mods, workerTraitName(clazz), tparams, transform(templ).asInstanceOf[Template])
          cdef setSymbol workerTraitSym //the tree of the virtual class shall become the tree of the worker trait
          
          List(mkAbstractTypeDef(clazz),
               mkFactoryDefDef(clazz.owner, clazz),
               localTyper.typed(cdef))
          
        case DefDef(mods, nme.CONSTRUCTOR, tparams, vparamss, tpt, rhs) //TODO ensure that constructor body is transferred to factory
          if (wasVirtualClass(sym.owner)) =>
          // if (atPhase(ownPhase)(sym != sym.owner.primaryConstructor))
          // unit.error(tree.pos, "virtual classes cannot have auxiliary constructors")
          val clazz = sym.owner
          val block = Block(List(), Literal(Constant(())) )
          val result = DefDef(mods, nme.MIXIN_CONSTRUCTOR, tparams, vparamss, TypeTree(definitions.UnitClass.tpe), block )
          val ctorsym = mixinCtor(clazz)
          result setSymbol ctorsym

          List(
          localTyper.typed {
            atPos(tree.pos) {
              result
            }
          })

        case _ =>
          List(transform(tree))
      }
    }

    override def transform(tree0 : Tree) : Tree = {
      val tree = super.transform(tree0)
      tree match {
        case app @ Apply(sel @ Select(New(tpt), nme.CONSTRUCTOR), args)
          if(wasVirtualClass(app.symbol.owner) &&  app.symbol.isConstructor) =>

          val clazz = app.symbol.owner

          localTyper.typed {
            atPos(tree.pos) {
              gen.mkMethodCall(Select(gen.mkAttributedQualifier(tpt.tpe.prefix),
                factoryName(clazz)),
                tpt.tpe.typeArgs,
                args)
            }
          }

        case _ => tree setType atPhase(ownPhase){ infoTransformer(tree.tpe) }
      }
    }
  }
}
