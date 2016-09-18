package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context
import java.util.concurrent.atomic.AtomicInteger

private[di] trait DagNodes[C <: Context] {
  val context: C

  import context.universe._

  lazy val reflectUtils = new ReflectUtils[context.type](context)

  object IdGen {
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
    
//    context.warning(sourcePos, "Ref:"+typ.toString)
  }

  sealed abstract case class DagNode(id: Int) extends DagNodeOrRef {
    def providerSource:ProviderSource
    def singletonName: TermName
    def invoke(inputs: Seq[Tree]): Tree
    def initialization: Seq[Tree] => Seq[Tree]
  }
  
  sealed trait ProviderSource
  
  object ProviderSource {
    
    case class MethodSource(method:MethodSymbol) extends ProviderSource
    case class ConstructorSource(method:MethodSymbol) extends ProviderSource
    case object ValueSource extends ProviderSource
    case class AllbindingsSource(itemType:TypeSymbol) extends ProviderSource
  }
  

  object DagNode {

    private final class DagNodeImpl private[DagNode] (
        lazyProviderSource: => ProviderSource,
        val kind: Kind,
        val description: String,
        val initialization: Seq[Tree] => Seq[Tree],
        val invoker: Seq[Tree] => Tree,
        val typ: Type,
        val sourcePos: Position,
        baseName: => String) extends DagNode(IdGen.next) {

      assert(typ != null, s"could not infer type on tree $description")

      def invoke(inputs: Seq[Tree]): Tree = invoker(inputs)
      lazy val singletonName = TermName(context.freshName(s"${baseName}Singleton"))

      lazy val providerSource = lazyProviderSource
    }

    def apply(
        providerSource:ProviderSource,
        kind: Kind,
      description: String,
      initialization: Seq[Tree] => Seq[Tree],
      invoker: Seq[Tree] => Tree,
      typ: Type,
      sourcePos: Position,
      baseName: => String): DagNode = {
      new DagNodeImpl(providerSource, kind, description, initialization, invoker, typ, sourcePos, baseName)
    }

    def value(kind: Kind, initialization: Seq[Tree], value: Tree, typ: Type, sourcePos: Position) = {
      apply(ProviderSource.ValueSource, 
          kind, s"$value", trees => initialization, (trees) => value, typ, sourcePos, typ.typeSymbol.name.decodedName.toString)
    }

    def methodCall(kind: Kind, containerTermName: Option[TermName], method: Symbol) = {
      lazy val methodSymbol = method.asMethod
      apply(new ProviderSource.MethodSource(method.asMethod),
        kind,
        s"$method", trees => Nil,
        inputs => reflectUtils.methodCall(containerTermName, methodSymbol, inputs),
        typ = methodSymbol.returnType,
        sourcePos = method.pos,
        method.name.decodedName.toString)
    }

    def constructorCall(kind: Kind, typ: Type, constructor: MethodSymbol, members: Seq[Tree]): DagNode = {
      val invoker: Seq[Tree] => Tree = if (members.isEmpty) { inputs =>
        reflectUtils.methodCall(None, constructor, inputs)
      } else { inputs =>
        reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs, members)
      }
      apply(new ProviderSource.ConstructorSource(constructor),
          kind, s"$constructor", trees => Nil, invoker, typ, constructor.pos, typ.typeSymbol.name.decodedName.toString)
    }

  }

  type Providers[T] = MProvidersMap[Id, T, DagNodeDagFactory]

  trait DagNodeDagFactory {

    def kind: Kind
    def apply(typ: Type): Option[Dag[DagNodeOrRef]]

  }

  class PolyDagNodeFactory(val kind: Kind, containerTermName: Option[TermName], method: MethodSymbol, polyType: PolyType, kindProvider: Symbol => Kinds) extends DagNodeDagFactory {

    val (typeArgs, underlying) = method.returnType match {
      case TypeRef(NoPrefix, underlying, args) => (args -> underlying)
      case TypeRef(prefix, underlying, args) => (args -> underlying)
      case _ => context.abort(context.enclosingPosition, s"not a poly type method $method, return type ${method.returnType.getClass}")
    }

    lazy val classSymbol = underlying
    lazy val underlyingTypeArgs = typeArgs //underlying.typeArgs

    override def toString = "PolyDagNodeFactory("+method.toString+")"
    
    def apply(concreteType: Type): Option[Dag[DagNodeOrRef]] = {
      if (concreteType.typeParams.nonEmpty) context.abort(context.enclosingPosition, s"type $concreteType is not a concrete type")
      if (concreteType.erasure != method.returnType.erasure) None
      else {

        val concrTypeArgs = concreteType.typeArgs

        if (concrTypeArgs.length != underlyingTypeArgs.length) context.abort(context.enclosingPosition, "concrTypeArgs.length != underlyingTypeArgs.length")

        val z: Option[List[(TypeSymbol, Type)]] = Some(Nil)

        val optSubstitutions = concrTypeArgs.zip(underlyingTypeArgs).foldLeft(z) {
          case (Some(mp), (concr, gen)) =>
            gen match {
              case TypeRef(NoPrefix, typVar, Nil) =>
                Some(mp :+ (gen.typeSymbol.asType -> concr))
              case gts =>
                if (concr.typeSymbol != gen) None
                else Some(mp)
            }
          case (None, _) => None
        }

        optSubstitutions.map { typesBinds =>

          val tpKeys = typesBinds.map(_._1)
          val tpVals = typesBinds.map(_._2)

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

          Node[DagNodeOrRef](
            DagNode(
                new ProviderSource.MethodSource(method),
                kind,
              s"$method[${substTypes.mkString(", ")}]",
              initialization = _ => Nil,
              invoker = { args => reflectUtils.methodCall(containerTermName, method, substTypes, args) },
              concreteType,
              method.pos,
              method.name.decodedName.toString),
            dagInputs)
        }
      }
    }

  }
}
