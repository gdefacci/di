package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context
import java.util.concurrent.atomic.AtomicInteger
import com.sun.javafx.fxml.expression.Expression

private[di] trait DagNodes[C <: Context] {
  val context: C

  import context.universe._

  lazy val reflectUtils = new ReflectUtils[context.type](context)
  val kindProvider = new DefaultKindProvider[context.type](context)

  object IdGen {
    private var counter = 0
    def next = {
      counter += 1
      counter
    }
  }

  sealed trait DagNodeOrRef {
    def kind: Kind
    def typ: Type
    def sourcePos: Position
    def description: String
    override def toString = description
  }

  sealed case class Ref(val kind: Kind, val typ: Type, val sourcePos: Position) extends DagNodeOrRef {
    assert(typ != null)
    def description: String = s"Reference to type $typ"
  }

  val Gen = new GenModel[Dag[DagNode], TermName, Tree]

  type DagToExpression = (Dag[DagNode], Seq[Dag[Gen.Expression]]) => Gen.Expression

  object DagToExpression {

    def apply(termName: TermName, value: Tree): DagToExpression = { (dag, deps) =>
      val decls = Gen.allDeclarations(deps.map(_.value), _.source.value.id)

      val v = new Gen.Value(dag, value)
      val decl = new Gen.Declaration(termName, v)
      new Gen.Block(dag, decls :+ decl, q"$termName")
    }

    def apply(valuef: Seq[Tree] => Tree): DagToExpression = { (dag, deps) =>
      val decls = Gen.allDeclarations(deps.map(_.value), _.source.value.id)
      new Gen.Block(dag, decls, valuef(deps.map(_.value.value)))
    }

    def const(tree: Tree): DagToExpression = { (dag, deps) =>
      new Gen.Value(dag, tree)
    }

    def expressionToTree(expr: Gen.Expression): Tree = {
      expr match {
        case v: Gen.Value => v.value
        case v: Gen.Block =>
          val decls = v.declarations.map { decl =>
            val nm = decl.name
            val v = expressionToTree(decl.expression)
            q"val $nm = $v"
          }
          q"""
          ..$decls
          ${v.value}
          """
      }
    }

    def function(funTyp: Type, paramsDags: Seq[(Tree, Dag[DagNode])]): DagToExpression = { (dag, deps) =>
      assert(deps.length == 1)

      val toInboundParameters = {
        val inboundParamsIds = paramsDags.map(_._2.value.id).toSet
        new DagConnections[DagNode]({ dg =>
          inboundParamsIds.contains(dg.id)
        })
      }

      val res = deps.head
      val isParamIndependentSingleton = (expr: Gen.Expression) => {
        expr.source.value.kind.scope == SingletonScope && !toInboundParameters.isConnected(expr.source)
      }

      val (impl, decls) = Gen.partitionContent(expr => !isParamIndependentSingleton(expr))(res.value)
      val pars = paramsDags.map(_._1)
      decls match {
        case Seq() => new Gen.Value(dag, q"(..$pars) => { ${expressionToTree(impl)} }")
        case decls => new Gen.Block(dag, decls, q"(..$pars) => { ${expressionToTree(impl)} }")
      }
    }

    class ConstructorCall(val constructor: MethodSymbol, val parametersDags: Seq[(Symbol, Dag[DagNode])])
    class ImplementedMethod(val method: MethodSymbol, val parametersDags: Seq[(Symbol, Set[Dag[DagNode]])], val impl: Dag[DagNode])

    //    def log(str: String) = context.warning(context.enclosingPosition, str)

    def abstractType(typ: Type,
      constructorCall: Option[ConstructorCall],
      implementedMethods: Seq[ImplementedMethod]): DagToExpression = { (dag, deps) =>

      val toInboundParameters = {
        val inboundParamsIds = implementedMethods.flatMap(_.parametersDags.flatMap(_._2.map(_.value.id))).toSet
        new DagConnections[DagNode]({ dg =>
          inboundParamsIds.contains(dg.id)
        })
      }

      val isParamIndependentSingleton = (d: Dag[DagNode]) => {
        d.value.kind.scope == SingletonScope && !toInboundParameters.isConnected(d)
      }

      val constrParLen = constructorCall.map(_.parametersDags.length).getOrElse(0)

      val (constrDeps, membDeps) = deps.splitAt(constrParLen)

      val members = membDeps.zip(implementedMethods).map {
        case (body, implMthd) =>
          val mthd = implMthd.method
          val args = mthd.paramLists.map { pars =>
            pars.map { par =>
              q"""${par.asTerm.name}: ${par.info}"""
            }
          }
          val (impl, decls) = Gen.partitionContent(expr => !isParamIndependentSingleton(expr.source))(body.value)

          val mthdTree = q"""def ${mthd.name}(...${args}):${mthd.returnType} = { 
             ${expressionToTree(impl)} 
          }"""

          decls match {
            case Seq() => new Gen.Value(dag, mthdTree)
            case decls => new Gen.Block(dag, decls, mthdTree)
          }
      }

      val allDecls = Gen.allDeclarations(constrDeps.map(_.value) ++ members, _.source.value.id)
      val resValue = constructorCall match {
        case None => reflectUtils.newTrait(typ.typeSymbol, members.map(_.value))
        case Some(constrCall) =>
          val constructor = constrCall.constructor
          reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, constrDeps.map(_.value.value), members.map(_.value))
      }
      allDecls match {
        case Seq() => new Gen.Value(dag, resValue)
        case decls => new Gen.Block(dag, decls, resValue)
      }
    }

    def singletonize(de: DagToExpression): DagToExpression = { (dag, deps) =>
      val r = de(dag, deps)
      if (r.source.value.kind.scope == SingletonScope) singletonizeExpression(r)
      else r
    }

    private def singletonizeExpression(e: Gen.Expression): Gen.Expression = {
      val singletonName = TermName(context.freshName("singleton" + e.source.value.name))
      e match {
        case v: Gen.Value =>
          new Gen.Block(v.source, new Gen.Declaration(singletonName, v) :: Nil, q"$singletonName")
        case v: Gen.Block =>
          val v1 = v.source.value
          val newSource =
            DagNode(v1.providerSource, v1.kind, v1.description, v1.name, v1.typ, v1.sourcePos, DagToExpression.const(q""))

          new Gen.Block(v.source, v.declarations :+ new Gen.Declaration(singletonName, new Gen.Value(Node(newSource, e.source.inputs), v.value)), q"$singletonName")
      }

    }

  }

  sealed abstract case class DagNode(id: Int) extends DagNodeOrRef {
    def name: String
    def providerSource: ProviderSource

    def dagToExpression: DagToExpression
  }

  sealed trait ProviderSource

  object ProviderSource {

    case class MethodSource(method: MethodSymbol) extends ProviderSource
    case class ConstructorSource(method: MethodSymbol) extends ProviderSource
    case object ValueSource extends ProviderSource
    case class AllbindingsSource(itemType: TypeSymbol) extends ProviderSource
  }

  object DagNode {

    private final class DagNodeImpl private[DagNode] (
        lazyProviderSource: => ProviderSource,
        val kind: Kind,
        val name: String,
        val description: String,
        val typ: Type,
        val sourcePos: Position,
        val dagToExpression: DagToExpression) extends DagNode(IdGen.next) {

      assert(typ != null, s"could not infer type on tree $description")

      lazy val providerSource = lazyProviderSource

    }

    def apply(
      providerSource: ProviderSource,
      kind: Kind,
      name: String,
      description: String,
      typ: Type,
      sourcePos: Position,
      dagToExpression: DagToExpression): DagNode = new DagNodeImpl(providerSource, kind, name, description, typ, sourcePos, dagToExpression)

    def value(kind: Kind, value: Tree, typ: Type, sourcePos: Position, dagToExpression: DagToExpression) =
      apply(ProviderSource.ValueSource, kind, s"$value", s"$value", typ, sourcePos, dagToExpression)

    def methodCall(kind: Kind, containerTermName: Option[TermName], method: Symbol) = {
      lazy val methodSymbol = method.asMethod
      val providerSource = new ProviderSource.MethodSource(method.asMethod)
      val mthdName = method.name.decodedName.toString
      val description = s"${method.owner.name}.${method.name}"
      apply(providerSource, kind, mthdName, description,
        typ = methodSymbol.returnType,
        sourcePos = method.pos,
        DagToExpression.apply(inputs => reflectUtils.methodCall(containerTermName, methodSymbol, inputs)))
    }

    def constructorCall(kind: Kind, typ: Type, constructor: MethodSymbol, members: Seq[Tree]): DagNode = {
      val invoker: Seq[Tree] => Tree = if (members.isEmpty) { inputs =>
        reflectUtils.methodCall(None, constructor, inputs)
      } else { inputs =>
        reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs, members)
      }
      val providerSource = new ProviderSource.ConstructorSource(constructor)
      val typName = typ.typeSymbol.name.decodedName.toString
      apply(providerSource, kind, typName, s"$constructor", typ, constructor.pos,
        DagToExpression(invoker))
    }

  }

  type Providers[T] = MProvidersMap[Id, T, DagNodeDagFactory]

  trait DagNodeDagFactory {

    def kind: Kind
    def apply(typ: Type): Option[Dag[DagNodeOrRef]]

  }

  class PolyDagNodeFactory(val kind: Kind, containerTermName: Option[TermName], method: MethodSymbol, polyType: PolyType) extends DagNodeDagFactory {

    val (typeArgs, underlying) = method.returnType match {
      case TypeRef(NoPrefix, underlying, args) => (args -> underlying)
      case TypeRef(prefix, underlying, args) => (args -> underlying)
      case _ => context.abort(context.enclosingPosition, s"not a poly type method $method, return type ${method.returnType.getClass}")
    }

    lazy val classSymbol = underlying
    lazy val underlyingTypeArgs = typeArgs //underlying.typeArgs

    override def toString = "PolyDagNodeFactory(" + method.toString + ")"

    def apply(concreteType: Type): Option[Dag[DagNodeOrRef]] = {
      if (concreteType.typeParams.nonEmpty) context.abort(context.enclosingPosition, s"type $concreteType is not a concrete type")
      if (concreteType.erasure != method.returnType.erasure) None
      else {

        val concrTypeArgs = concreteType.typeArgs

        if (concrTypeArgs.length != underlyingTypeArgs.length) context.abort(context.enclosingPosition, "concrTypeArgs.length != underlyingTypeArgs.length")

        val z: Option[(List[TypeSymbol], List[Type])] = Some(Nil, Nil)

        val optSubstitutions = concrTypeArgs.zip(underlyingTypeArgs).foldLeft(z) {
          case (Some((ls, ts)), (concr, gen)) =>
            gen match {
              case TypeRef(NoPrefix, typVar, Nil) =>
                Some((ls :+ gen.typeSymbol.asType) -> (ts :+ concr))
              case gts =>
                if (concr.typeSymbol != gen) None
                else Some(ls -> ts)
            }
          case (None, _) => None
        }

        optSubstitutions.map { typesBinds =>

          val (tpKeys, tpVals) = typesBinds

          val substTypes = polyType.typeParams.map { tp =>
            tp.asType.toType.substituteTypes(tpKeys, tpVals)
          }
          val dagInputs = method.paramLists.flatMap(pars => pars.map { par =>
            val parKnd = kindProvider(par)
            if (parKnd.ids.size > 1) {
              context.abort(par.pos, "parameters must have at most one identifier annotation")
            }
            if (parKnd.scope != DefaultScope) {
              context.abort(par.pos, "parameters cant have scope annotations")
            }
            val parTyp = par.info.substituteTypes(tpKeys, tpVals)
            Leaf[DagNodeOrRef](Ref(Kind(parKnd.ids.head, parKnd.scope), parTyp, par.pos))
          })

          val mRetType = polyType.resultType.substituteTypes(tpKeys, tpVals)

          val providerSource = new ProviderSource.MethodSource(method)
          val description = s"$method[${substTypes.mkString(", ")}]"
          val invoker: Seq[Tree] => Tree = { args => reflectUtils.methodCall(containerTermName, method, substTypes, args) }

          val nd =
            DagNode(providerSource, kind, method.name.toString, description,
              concreteType,
              method.pos,
              DagToExpression(invoker))

          Node[DagNodeOrRef](nd, dagInputs)
        }
      }
    }

  }
}
