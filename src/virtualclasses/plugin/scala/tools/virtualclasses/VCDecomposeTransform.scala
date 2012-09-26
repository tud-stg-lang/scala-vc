package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform._


abstract class VCDecomposeTransform(val global: Global) extends PluginComponent with Transform
  with TypingTransformers with InfoTransform with Commons  {

  import global._

  override val phaseName = "vc_decompose"

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
        if (isClassFamily(clazz)) =>
          //perform decomposition of class family

          val decls = new Scope
          def enter(sym: Symbol) =
            atPhase(ownPhase.next) { decls.enter(sym) }
                   
          for (m <- decls0.toList) {
            if (m.isVirtualClass) {
              //in this case we replace the entry with workertrait+abstract type+factory symbols
              val workerTrait = mkWorkerTraitSym(m)
              val abstpe = mkAbstractTypeSym(clazz, m, workerTrait.tpe)
              val factory = mkFactorySym(clazz, m, abstpe.tpe)

              enter(abstpe)
              enter(factory)
              //do NOT put original symbol of virtual class back
              //having virtual class and trait as separate symbols
              //allows us to identify if a tree's symbol is a virtual class
              //later in the TypingTransformer
              enter(workerTrait)
            }
            else enter(m)
          }

          clazz setFlag TRAIT

          ClassInfoType(parents map this, decls, clazz)

      case ClassInfoType(parents, decls, clazz)
        if (clazz.isVirtualClass) =>
           val workerTrait = atPhase(ownPhase.next) { clazz.owner.info.member(workerTraitName(clazz)) }
           val info = workerTrait.info.asInstanceOf[ClassInfoType]
           ClassInfoType(info.parents map this, mapOver(info.decls), info.typeSymbol)

      case ThisType(clazz)
        if (clazz.isVirtualClass) =>
           val workerTrait = atPhase(ownPhase.next) { clazz.owner.info.member(workerTraitName(clazz)) }
           ThisType(workerTrait)

      case tp1 @ TypeRef(pre, clazz, args)
        if clazz.isVirtualClass =>
           typeRef(this(pre), abstractType(clazz.owner, clazz.name), args map this)

      case x =>
        x

      }
    }

    /**
     * Worker trait symbol out of given virtual class symbol.
     *
     * The resulting symbol's info shares the parents and
     * decls fields with the virtual class symbol.
     */
    private def mkWorkerTraitSym(clazz: Symbol): Symbol = {
      val workerTrait = clazz.cloneSymbol(clazz.owner)
      workerTrait.setFlag(TRAIT | SYNTHETIC)
      workerTrait.resetFlag(DEFERRED)
      workerTrait.name = workerTraitName(clazz)

      val info = clazz.info.asInstanceOf[ClassInfoType]
      //At this point workerTrait shares info instance x with sym, but x.typeSymbol
      //refers to sym instead of workerTrait and cannot be modified.
      //Hence we must give workerTrait new ClassInfo with proper typeSymbol field.
      //We do NOT map over parents and decls yet, for this see the 2nd
      //case for ClassInfoType in apply.
      workerTrait setInfo new ClassInfoType(info.parents, info.decls, workerTrait)
      fixCtors(workerTrait)
      workerTrait
    }

    /**
     * Remove constructor symbols and add mixin-ctor symbol
     * for newly created worker trait symbol.
     *
     * TODO how to handle multiple ctors?
     * */
    private def fixCtors(workerTrait : Symbol) {
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
      val abstpe = owner.info.decl(newTypeName(ABSTPEPREFIX+name))
      assert(abstpe.isAbstractType)
      abstpe
    }
  }

  protected def mkAbstractTypeSym(owner : Symbol, clazz : Symbol, upperBound : Type) : Symbol = {
    val absTpe =
      //owner.newAbstractType(sym.pos, sym.name.asInstanceOf[TypeName])
      owner.newAbstractType(clazz.pos, newTypeName(ABSTPEPREFIX+clazz.name))
        .setFlag(clazz.flags & (AccessFlags | DEFERRED) | SYNTHETIC)
        .setAnnotations(clazz.annotations)

   // atPhase(ownPhase.next) {
      absTpe.setInfo(TypeBounds(definitions.NothingClass.tpe, upperBound))
      absTpe
    //}
  }

  protected def mkFactorySym(owner: Symbol, clazz: Symbol, abstpe: Type): Symbol = {
    atPhase(ownPhase) {
      val factorySym = owner.newMethod(clazz.pos, factoryName(clazz))
      val tparams =  clazz.typeParams map (_.cloneSymbol(factorySym))

      def cloneAndSubst(s : Symbol) : Symbol = {
        s.cloneSymbol(factorySym).setInfo(
          s.tpe.substSym(clazz.typeParams, tparams))
      }
      val params = clazz.primaryConstructor.paramss flatMap  ( _.map (cloneAndSubst))
      factorySym.setInfo(polyType(tparams, MethodType(params, abstpe.substSym(abstpe.typeParams, tparams))))
      factorySym.setFlag(clazz.flags & AccessFlags | SYNTHETIC | DEFERRED)
      factorySym
    }
  }

  def newTransformer(unit: CompilationUnit) = new VCTransformer(unit)

  protected def isClassFamily(sym: Symbol) = sym.info.decls exists (_.isVirtualClass)
  protected def getVirtualClassSymbols(sym: Symbol) = atPhase(ownPhase) { sym.info.decls.toList filter (_.isVirtualClass) }

  class VCTransformer(val unit: CompilationUnit) extends TypingTransformer(unit) {

    protected def addOverriddenVirtuals(clazz: Symbol) = {
      (clazz.allOverriddenSymbols filter (_.isVirtualClass)) ::: List(clazz)
    }

    /** The factory symbol corresponding to a virtual class. */
    protected def factory(owner: Symbol, clazz : Symbol) = {
      atPhase(ownPhase.next) {
        val fsym = owner.info.member(factoryName(clazz))
        assert(fsym.isMethod, clazz)
        fsym
      }
    }

    /** The worker trait symbol corresponding to a virtual class. */
    protected def workerTrait(clazz : Symbol) : Symbol = {
      workerTrait(clazz.owner, clazz)
    }

    /** The worker trait symbol corresponding to a virtual class. */
    protected def workerTrait(owner: Symbol, clazz : Symbol) : Symbol = {
      atPhase(ownPhase.next) {
        val wtsym = owner.info.member(workerTraitName(clazz))
        assert(wtsym.isTrait, clazz)
        wtsym
      }
    }

    /** The mixin constructor symbol of the workertrait for given virtual class */
    protected def mixinCtor(clazz : Symbol) = {
      atPhase(ownPhase.next) {
        val workerTraitSym = workerTrait(clazz)
        val csym = workerTraitSym.info.member(nme.MIXIN_CONSTRUCTOR)
        assert(csym.isConstructor, clazz)
        csym
      }
    }

    protected def wasVirtualClass(clazz : Symbol) = {
      atPhase(ownPhase) {
        //This works only because we do not recycle symbols of virtual classes for worker traits.
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
      localTyper.typed {
        atPos(clazz.pos) {
            DefDef(factorySym, Modifiers(factorySym.flags), EmptyTree)
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

          val workerTraitSym = workerTrait(clazz)
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

    /**
     * Decides if given symbol's owner needs to be replaced by worker trait symbol.
     */
    protected def needsOwnerReplaced(sym: Symbol): Boolean = {
      (sym != null) && (sym ne NoSymbol) && (sym.owner != null) && wasVirtualClass(sym.owner)
    }


    /**
      * Transformation in post-order.
      *
      * Eliminates leftover traces of original virtual class symbols:
      *
      * - Fixes the owner of tree symbols:
      * Trees owned by virtual classes should
      * be owned by worker traits.
      * We keep this separate from @see preTransform
      * because replacement of new calls breaks otherwise.
      * - Changes the types of tree symbols:
      * e.g. the symbols of ValDef and DefDef trees as their info
      * might contain virtual class refs.
      *
      * See also transformStats, preTransform, transform
      *
      * @param tree
      * @return
      */
    protected def postTransform(tree : Tree) : Tree = {
      val sym = tree.symbol
      //fix owners
      if(needsOwnerReplaced(sym)) {
        val clazz = sym.owner
        val workerTraitSym = workerTrait(clazz)
        sym.owner = workerTraitSym
      }
      //fix symbol types
      if(sym != null)
        sym.info = atPhase(ownPhase) {
          infoTransformer(sym.info)
        }

      tree
    }

    /**
      * Transformation in pre-order.
      *
      * Performs transformations that arise from the
      * trait/factory/type split-up of virtual classes:
      *
      * - Changes the type of trees via @see infoTransformer
      * - Replaces new with call to factory
      * - Fixes type and self references
      *
      * See also transformStats, postTransform, transform
      *
      * @param tree
      * @return
      */
    protected def preTransform(tree : Tree) : Tree = {
      tree match {
        //replace new calls with factory calls
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

        //rename local dummy from <local X> to <local VC_TRAIT$X>
        case tmpl @ Template(_, _, _)
          if(wasVirtualClass(tmpl.symbol.owner)) =>
          val clazz = tmpl.symbol.owner
          val sym = tmpl.symbol
          tmpl.setSymbol(sym.cloneSymbol(clazz))
          tmpl.symbol.name = nme.localDummyName(workerTrait(clazz.owner, clazz))
          tmpl

        //type ref. virtual class -> type ref. abstract type
        case tpt @ TypeTree()
          if(wasVirtualClass(tpt.symbol)) =>
          localTyper.typed {
            atPos(tpt.pos) {
              TypeTree(abstractType(tpt.symbol).tpe)
            }
          }

        //self reference virtual class -> self ref. worker trait
        case ths @ This(qual)
          if(wasVirtualClass(ths.symbol)) =>
          val workerTraitSym = ths.symbol.owner.info.member(workerTraitName(qual))
          val newThs = This(workerTraitName(qual)).setSymbol(workerTraitSym)
          localTyper.typed {
            atPos(ths.pos) {
              newThs
            }
          }

        case _ =>
          tree setType atPhase(ownPhase){
             infoTransformer(tree.tpe)
          }
      }
    }

    override def transform(tree0 : Tree) : Tree = {
      postTransform(super.transform(preTransform(tree0)))
    }
  }
}
