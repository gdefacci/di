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
  }

  sealed abstract case class DagNode(id: Int) extends DagNodeOrRef {
    def singletonName: TermName
    def invoke(inputs: Seq[Tree]): Tree
    def initialization: Seq[Tree] => Seq[Tree]
  }

  object DagNode {

    private final class DagNodeImpl(val kind: Kind,
        val description: String,
        val initialization: Seq[Tree] => Seq[Tree],
        val invoker: Seq[Tree] => Tree,
        val typ: Type,
        val sourcePos: Position,
        baseName: => String) extends DagNode(IdGen.next) {

      assert(typ != null, s"could not infer type on tree $description")

      def invoke(inputs: Seq[Tree]): Tree = invoker(inputs)
      lazy val singletonName = TermName(context.freshName(s"${baseName}Singleton"))

    }

    def apply(kind: Kind,
      description: String,
      initialization: Seq[Tree] => Seq[Tree],
      invoker: Seq[Tree] => Tree,
      typ: Type,
      sourcePos: Position,
      baseName: => String): DagNode = {
      new DagNodeImpl(kind, description, initialization, invoker, typ, sourcePos, baseName)
    }

    def value(kind: Kind, initialization: Seq[Tree], value: Tree, typ: Type, sourcePos: Position) = {
      apply(kind, s"$value", trees => initialization, (trees) => value, typ, sourcePos, typ.typeSymbol.name.decodedName.toString)
    }

    def methodCall(kind: Kind, containerTermName: Option[TermName], method: Symbol) = {
      lazy val methodSymbol = method.asMethod
      apply(kind,
        s"$method", trees => Nil,
        inputs => reflectUtils.methodCall(containerTermName, methodSymbol, inputs),
        typ = methodSymbol.returnType,
        sourcePos = method.pos,
        method.name.decodedName.toString)
    }

    def constructorCall(kind: Kind, containerTermName: Option[TermName], typ: Type, constructor: MethodSymbol, members: Seq[Tree]): DagNode = {
      val invoker: Seq[Tree] => Tree = if (members.isEmpty) { inputs =>
        reflectUtils.methodCall(containerTermName, constructor, inputs)
      } else { inputs =>
        reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs, members)
      }
      apply(kind, s"$constructor", trees => Nil, invoker, typ, constructor.pos, typ.typeSymbol.name.decodedName.toString)
    }

  }

  private def reportDuplicateMapping[T](id: Id, typeSymbol: Symbol, dags: Seq[Dag[T]]): Nothing = {
    val text =
      s"""
duplicates bindings: ${dags.length}
type ${typeSymbol} ${id} has more than one binding
${
        dags.map { dupEntry =>
          s"""
${dupEntry}
"""
        }.mkString("")
      }
"""
    context.abort(context.enclosingPosition, text)
  }

  type Providers[T] = ProvidersMap[Id, T, DagNodeDagFactory]

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
            Leaf[DagNodeOrRef](Ref(Kind(parKnd.ids.head, parKnd.scope), par.info.substituteTypes(tpKeys, tpVals), par.pos))
          })

          val mRetType = polyType.resultType.substituteTypes(tpKeys, tpVals)

          Node[DagNodeOrRef](
            DagNode(kind,
              s"$method[${substTypes.mkString(", ")}]",
              initialization = _ => Nil,
              invoker = { args => reflectUtils.methodCall(containerTermName, method, substTypes, args) },
              mRetType,
              method.pos,
              method.name.decodedName.toString),
            dagInputs)
        }
      }
    }

  }
}
