package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox

trait TypeResolverMixin[C <: blackbox.Context] { self: DagNodes[C] with DagNodeOrRefFactory[C] with DagToExpressionFactoryMixin[C] =>

  import DagToExpressionFactory.{ ConstructorCall, ImplementedMethod }

  import context.universe._

  val membersSelect = new MembersSelect[context.type](context)

  private def describeId(id: Id) = id match {
    case Global | Derived => ""
    case WithName(nm) => s"with name $nm"
    case WithQualifier(nm, mp) => s"with qualifier $nm (${mp.mkString(", ")})"
  }

  class Stacker private (stack: IndexStack[DagNodeOrRef]) {

    def message: String = {
      stack.map(_.typ).distinct.map(hd => s"required by ${hd}").mkString("\n")
    }

    def error(msg: String) = {
      context.abort(context.enclosingPosition, msg +" "+ this.message)
    }

    def withStack(nd: DagNodeOrRef)(res: => Dag[DagNode]) = {
      stack.push(nd, () => {
        val typ = nd.typ
        val id = nd.kind.id
        error(s"cycle detected with type $typ ${describeId(id)}")        
      })
      val r = res
      stack.pop
      r
    }

    def copy() = new Stacker(stack.copy())

  }
  
  object Stacker {
    def empty = new Stacker(new IndexStack[DagNodeOrRef])
  }

  class TypeResolver(
      mappings: Providers[DagNodeOrRef],
      dagProviders: MapOfBuffers[Id, Dag[DagNode]],
      topLevelRefs: Set[Ref],
      stack: Stacker = Stacker.empty) {

    private val currentSingletons: MapOfBuffers[Id, Dag[DagNode]] = MapOfBuffers.empty

    private def error(msg: String) = {
      stack.error(msg)
    }

    private def missingBindingError(id: Id, typ: Type) = {
      error(s"could not find a binding for $typ ${describeId(id)}")
    }

    def resolveDagNodeOrRef(nd: DagNodeOrRef, inputs: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = {
      nd match {
        case ref @ Ref(_, _, _) =>
          assert(inputs.isEmpty, "refs must have no inputs")
          resolveRef(ref)
        case nd: DagNode => resolveDagNode(nd, inputs)
      }
    }

    private def resolveRef(ref: Ref): Dag[DagNode] = {
      val Ref(Kind(id, _), typ, _) = ref

      val rs = dagProviders.find(id, nd => nd.value != ref && nd.value.typ <:< typ)
      rs match {
        case Seq() =>
          val r = membersSelect.multiTargetItem(typ) match {

            case Some(itemType) => resolveMultiTargetRef(ref, itemType)
            case None => resolveTargetRef(ref)

          }
          if (dagProviders.find(id, nd => nd.value.typ <:< typ).isEmpty) {
            if (r.value.kind.scope == SingletonScope) currentSingletons += (id, r)
            dagProviders += (id, r)
          }
          r
        case Seq(hd) => hd
        case items => error(s"more than one instance of $typ ${describeId(id)}: ${items.map(_.value).mkString(", ")}")
      }
    }

    private def resolveTargetRef(ref: Ref): Dag[DagNode] = {
      val Ref(Kind(id, _), typ, pos) = ref
      val typMappings = mappings.findMembers(id, { nd => nd != ref && nd.typ <:< typ })
      typMappings match {
        case Seq() =>
          val polyMembers = mappings.findPolymorphicMembers(id, df => df.apply(typ))
          polyMembers match {
            case Seq() => instantiateRef(ref)
            case Seq(dag) => resolveDagNodeOrRef(dag.value, dag.inputs)
            case _ => error(s"found more than a polymorphic factory for $typ ${describeId(id)}")
          }

        case Seq(dag) => resolveDagNodeOrRef(dag.value, dag.inputs)
        case _ => error(s"found more than a polymorphic factory for $typ ${describeId(id)}")
      }
    }

    private def resolveMultiTargetRef(ref: Ref, itemType: Type):Dag[DagNode] = {
      val Ref(Kind(id, _), typ, pos) = ref
      val insts: Seq[Dag[DagNodeOrRef]] = mappings.findMembers(id, (nd) => nd != ref && nd.typ <:< itemType)
      val desc = s"allBindings$itemType"
      val nd = DagNode(new ProviderSource.AllbindingsSource(itemType.typeSymbol.asType),
        Kind.default,
        desc, desc,
        typ, pos,
        DagToExpression(inps => q"new com.github.gdefacci.di.runtime.AllBindings[$itemType]( List[$itemType](..$inps) )"))

      resolveDagNode(nd, insts)
    }

    private def resolveDagNode(nd: DagNode, inputs: Seq[Dag[DagNodeOrRef]]): Dag[DagNode] = {
      stack.withStack(nd) {
        Dag[DagNode](nd, inputs.map(dg => resolveDagNodeOrRef(dg.value, dg.inputs)))
      }
    }

    private def instantiateRef(ref: Ref):Dag[DagNode] = {
      val Ref(Kind(id, _), typ, pos) = ref

      lazy val noBindingError = missingBindingError(id, typ)

      if (membersSelect.isPrimitive(typ)) {
        noBindingError
      }

      stack.withStack(ref) {
        lazy val isTopLevelref = topLevelRefs.contains(ref)

        if (reflectUtils.isFunctionType(typ.typeSymbol)) {
          if (isTopLevelref) implementsFunction(ref.kind, typ, ref.sourcePos)
          else noBindingError
        } else if (typ.typeSymbol.isAbstract) {
          if (isTopLevelref) implementsAbstractType(ref.kind, typ)
          else noBindingError
        } else {
          val constructorMethod = membersSelect.getPrimaryConstructor(typ).getOrElse {
            error(s"cant find primary constructor for ${typ.typeSymbol.fullName} resolving $typ ${describeId(id)}")
          }
          membersSelect.getPolyType(constructorMethod.returnType.etaExpand).map { polyType =>
            val dg = new PolyDagNodeFactory(ref.kind, None, constructorMethod, polyType).apply(ref.typ).getOrElse {
              error(s"error creating dag for polymorpic primary constructor for ${typ.typeSymbol.fullName} resolving $typ ${describeId(id)}")
            }
            resolveDagNodeOrRef(dg.value, dg.inputs)
          }.getOrElse {
            val dnd = constructorDag(ref.kind, typ, constructorMethod, Nil)
            resolveDagNodeOrRef(dnd.value, dnd.inputs)
          }
        }
      }
    }

    private def implementsFunction(kind: Kind, funTyp: Type, pos: Position): Dag[DagNode] = {

      val inpTypes = funTyp.typeArgs.init
      val parnames = 1.to(inpTypes.length).map(i => TermName(s"par$i"))
      val pars = inpTypes.zip(parnames).map { case (inp, parName) => q"$parName:$inp" }
      val parametersDags: Seq[Dag[DagNode]] = {
        inpTypes.zip(parnames).map { case (inp, par) => Dag(DagNode.value(Kind.default, q"$par", inp, pos, DagToExpression.const(q"$par"))) }
      }
      val res = {
        val resultType = funTyp.typeArgs.last
        resolveSeparate(resultType, parametersDags.map(nd => nd.value.kind.id -> nd))
      }
      val ftsym = funTyp.typeSymbol
      val name: String = ftsym.name.toString
      val description = ftsym.fullName
      Dag(DagNode(ProviderSource.ValueSource, kind, name, description, funTyp, ftsym.pos, DagToExpressionFactory.function(funTyp, pars.zip(parametersDags))),
        res :: Nil)
    }

    private def implementsAbstractType(kind: Kind, typ: Type): Dag[DagNode] = {

      val primaryConstructor = membersSelect.getPrimaryConstructor(typ)
      val constrPars = primaryConstructor.map(c => c -> c.paramLists.flatten).map {
        case (constr, pars) => new ConstructorCall(constr, pars.map(par => par -> resolveDagNodeOrRef(outboundParameterDag(Kind.default, par), Nil)))
      }
      val pars = membersSelect.abstractMembers(typ).map { mthd =>
        val (pdgs, impl) = implementMethod(mthd)
        new ImplementedMethod(mthd, pdgs, impl)
      }
      val tsym = typ.typeSymbol
      val name: String = tsym.name.toString
      val description = tsym.fullName
      val pos = tsym.pos

      Dag(DagNode(ProviderSource.ValueSource, kind, name, description, typ, pos, DagToExpressionFactory.abstractType(typ, constrPars, pars)),
        constrPars.map(_.parametersDags.map(_._2)).getOrElse(Nil) ++ pars.map(_.impl))
    }

    private def inboundParameterDag(par: Symbol, knd: Kind): Dag[DagNode] =
      Dag[DagNode](DagNode.value(knd, q"${par.asTerm.name}", par.info, par.pos, DagToExpression.const(q"${par.asTerm.name}")))

    private def implementMethod(m: MethodSymbol): (List[(Symbol, Set[Dag[DagNode]])], Dag[DagNode]) = {
      val pars0 = m.paramLists.flatMap { pars =>
        pars.map { par =>
          val knd = kindProvider(par)
          par -> knd.ids.map(id => id -> inboundParameterDag(par, Kind(id, knd.scope)))
        }
      }
      val pars = pars0.map {
        case (par, nds) => par -> nds.map(_._2)
      }
      val parametersBindings = pars0.flatMap(_._2)
      val typ = m.returnType
      pars -> resolveSeparate(typ, parametersBindings)
    }

    private def resolveSeparate(typ: Type, parametersBindings: Seq[(Id, Dag[DagNodeOrRef])]): Dag[DagNode] = {
      val nmappings: Providers[DagNodeOrRef] = mappings.copy()
      nmappings ++= parametersBindings
      val dagProviders = this.dagProviders.copy

      val tr = new TypeResolver(nmappings, dagProviders, topLevelRefs, stack.copy())
      val res = tr.resolveDagNodeOrRef(Ref(Kind(Global, DefaultScope), typ, typ.typeSymbol.pos), Nil)
      this.dagProviders ++= tr.currentSingletons
      res
    }

  }

}