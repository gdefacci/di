package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context
import java.util.concurrent.atomic.AtomicInteger

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

  object IdGenAtomic {
    private val counter = new java.util.concurrent.atomic.AtomicInteger(0)
    def next = counter.incrementAndGet
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

  sealed abstract case class DagNode(id: Int) extends DagNodeOrRef {
    def name: String
    def providerSource: ProviderSource
  }

  sealed abstract class SimpleDagNode(id: Int) extends DagNode(id) {
    def invoke(inputs: Seq[Tree]): Tree
    def initialization: Seq[Tree] => Seq[Tree]
  }

  sealed abstract class AbstractTypeDagNode(id: Int) extends DagNode(id) {
    def initialization(dependencies: Seq[DagToTree]): Seq[Tree]
    def invoke(inputs: Seq[DagToTree]): Tree
  }

  sealed trait ProviderSource

  object ProviderSource {

    case class MethodSource(method: MethodSymbol) extends ProviderSource
    case class ConstructorSource(method: MethodSymbol) extends ProviderSource
    case object ValueSource extends ProviderSource
    case class AllbindingsSource(itemType: TypeSymbol) extends ProviderSource
  }

  trait DagToTree {
    def id: Int

    def dag: Dag[DagNode]
    def dependencies: Seq[DagToTree]
    def initialization: Seq[Tree]
    def localInitialization: Seq[Tree]

    def value: Tree

    def allDependencies = DagToTree.distinct(dependencies)
  }

  object DagToTree {
    import collection.mutable.{ Set => MSet }

    def distinct(dependencies: Seq[DagToTree], visited: MSet[Int] = MSet.empty): Seq[DagToTree] = {
      dependencies.flatMap { d =>
        if (visited.contains(d.id)) Nil
        else {
          visited += d.id
          val res1 = distinct(d.dependencies, visited)
          res1 :+ d
        }
      }
    }

  }

  object AbstractTypeDag {

    final class AbstractTypeDagNodeImpl1 (
        val kind: Kind,
        val typ: Type,
        initializer:Seq[DagToTree] => Seq[Tree],
        invoker:Seq[DagToTree] => Tree) extends AbstractTypeDagNode(IdGen.next) {

      val name: String = typ.typeSymbol.name.toString
      val description = typ.typeSymbol.fullName

      lazy val providerSource = ProviderSource.ValueSource

      val sourcePos: Position = typ.typeSymbol.pos
      assert(typ != null, s"could not infer type on tree $description")
      
      def initialization(dependencies: Seq[DagToTree]): Seq[Tree] = initializer(dependencies) 
      
      def invoke(deps: Seq[DagToTree]): Tree = invoker(deps) 
        
    }

  }

  object DagNode {

    private final class DagNodeImpl private[DagNode] (
        lazyProviderSource: => ProviderSource,
        val kind: Kind,
        val name: String,
        val description: String,
        val initialization: Seq[Tree] => Seq[Tree],
        invoker: Seq[Tree] => Tree,
        val typ: Type,
        val sourcePos: Position) extends SimpleDagNode(IdGen.next) {

      assert(typ != null, s"could not infer type on tree $description")

      def invoke(inputs: Seq[Tree]): Tree = invoker(inputs)

      lazy val providerSource = lazyProviderSource
    }

    def apply(
      providerSource: ProviderSource,
      kind: Kind,
      name: String,
      description: String,
      initialization: Seq[Tree] => Seq[Tree],
      invoker: Seq[Tree] => Tree,
      typ: Type,
      sourcePos: Position): DagNode = new DagNodeImpl(providerSource, kind, name, description, initialization, invoker, typ, sourcePos)

    def value(kind: Kind, initialization: Seq[Tree], value: Tree, typ: Type, sourcePos: Position) =
      apply(ProviderSource.ValueSource, kind, s"$value", s"$value", trees => initialization, trees => value, typ, sourcePos)

    def methodCall(kind: Kind, containerTermName: Option[TermName], method: Symbol) = {
      lazy val methodSymbol = method.asMethod
      val providerSource = new ProviderSource.MethodSource(method.asMethod)
      val mthdName = method.name.decodedName.toString
      val description = s"${method.owner.name}.${method.name}"
      apply(providerSource, kind, mthdName, description,
        trees => Nil,
        inputs => reflectUtils.methodCall(containerTermName, methodSymbol, inputs),
        typ = methodSymbol.returnType,
        sourcePos = method.pos)
    }

    def constructorCall(kind: Kind, typ: Type, constructor: MethodSymbol, members: Seq[Tree]): DagNode = {
      val invoker: Seq[Tree] => Tree = if (members.isEmpty) { inputs =>
        reflectUtils.methodCall(None, constructor, inputs)
      } else { inputs =>
        reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs, members)
      }
      val providerSource = new ProviderSource.ConstructorSource(constructor)
      val typName = typ.typeSymbol.name.decodedName.toString
      apply(providerSource, kind, typName, s"$constructor", trees => Nil, invoker, typ, constructor.pos)
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
              initialization = _ => Nil,
              invoker = invoker,
              concreteType,
              method.pos)

          Node[DagNodeOrRef](nd, dagInputs)
        }
      }
    }

  }
}
