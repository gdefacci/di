package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodes[C <: Context] {
  val context:C

  import context.universe._

  lazy val reflectUtils = new ReflectUtils[context.type](context)

  sealed trait DagNodeOrRef {
    val kind: Kind
    def typ: Type
    def sourcePos: Position
    def description: String
    val initialization: Seq[Tree]
  }

  sealed trait DagNode extends DagNodeOrRef

  sealed trait MethodNode extends DagNode {
    def invoke(inputs:Seq[Tree]):Tree
  }

  sealed trait ValueNode extends DagNode {
    val value: Tree
//    def sourceTree: Tree
  }

  case class MethodDagNode(val kind: Kind, val containerTermName: Option[TermName], val method: Symbol, initialization: Seq[Tree]) extends MethodNode {
    val methodSymbol = method.asMethod
    val typ = methodSymbol.returnType
    val sourcePos: Position = method.pos

    def description: String = s"$method"

    def invoke(inputs:Seq[Tree]):Tree =
      reflectUtils.methodCall(containerTermName, methodSymbol, inputs)
  }

  case class ConstructorDagNode(val kind: Kind, val containerTermName: Option[TermName], val method: MethodSymbol, members:Seq[Tree], initialization: Seq[Tree]) extends MethodNode {
    val typ = method.returnType
    val sourcePos: Position = method.pos

    def description: String = s"$method"

    def invoke(inputs:Seq[Tree]):Tree =
      if (members.isEmpty) reflectUtils.methodCall(containerTermName, method, inputs)
      else reflectUtils.newAbstractClass(method.owner, method.paramLists, inputs, members)
  }

  case class ValueDagNode(kind: Kind, initialization: Seq[Tree], value: Tree, typ: Type, sourcePos:Position) extends ValueNode {
//    lazy val sourcePos: Position = sourceTree.pos
    def description: String = s"$value"
  }

  case class ParameterDagNode(val kind: Kind, val value: Tree, val typ: Type, val sourceTree: Tree) extends ValueNode {
    val initialization: Seq[Tree] = Nil
    lazy val sourcePos: Position = sourceTree.pos
    def description: String = s"$sourceTree"
  }

  case class Ref(val kind: Kind, val typ: Type, val sourcePos: Position) extends DagNodeOrRef {
    def description: String = s"Reference to type $typ"
    val initialization: Seq[Tree] = Nil
  }

}
