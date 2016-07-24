package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

private[di] trait DagNodes[C <: Context] {
  val context:C

  import context.universe._

  lazy val reflectUtils = new ReflectUtils[context.type](context)
  
  object IdGen {
    private val counter = new java.util.concurrent.atomic.AtomicInteger(0)
    def next = counter.incrementAndGet
  }

  sealed trait DagNodeOrRef {
    val kind: Kind
    def typ: Type
    def sourcePos: Position
    def description: String
  }

  sealed abstract case class DagNode(id:Int) extends DagNodeOrRef

  sealed trait MethodNode extends DagNode {
    def invoke(inputs:Seq[Tree]):Tree
  }

  sealed trait ValueNode extends DagNode {
    val value: Tree
    def initialization:Seq[Tree]
//    def sourceTree: Tree
  }

  final class MethodDagNode(val kind: Kind, val containerTermName: Option[TermName], val method: Symbol, id:Int = IdGen.next) extends DagNode(id) with MethodNode {
    val methodSymbol = method.asMethod
    val typ = methodSymbol.returnType
    val sourcePos: Position = method.pos

    def description: String = s"$method"

    def invoke(inputs:Seq[Tree]):Tree =
      reflectUtils.methodCall(containerTermName, methodSymbol, inputs)
  }

  final class ConstructorDagNode(val kind: Kind, val containerTermName: Option[TermName], val method: MethodSymbol, 
                                 val members:Seq[Tree], id:Int = IdGen.next) extends DagNode(id) with MethodNode {
    val typ = method.returnType
    val sourcePos: Position = method.pos

    def description: String = s"$method"

    def invoke(inputs:Seq[Tree]):Tree =
      if (members.isEmpty) reflectUtils.methodCall(containerTermName, method, inputs)
      else reflectUtils.newAbstractClass(method.owner, method.paramLists, inputs, members)
  }

  final class ValueDagNode(val kind: Kind, val initialization: Seq[Tree], val value: Tree, val typ: Type, 
                           val sourcePos:Position, id:Int = IdGen.next) extends DagNode(id) with ValueNode {
//    lazy val sourcePos: Position = sourceTree.pos
    def description: String = s"$value"
  }

  final class ParameterDagNode(val kind: Kind, val value: Tree, val typ: Type, val sourceTree: Tree, id:Int = IdGen.next) extends DagNode(id) with ValueNode {
    val initialization: Seq[Tree] = Nil
    lazy val sourcePos: Position = sourceTree.pos
    def description: String = s"$sourceTree"
  }

  case class Ref(val kind: Kind, val typ: Type, val sourcePos: Position) extends DagNodeOrRef {
    def description: String = s"Reference to type $typ"
    val initialization: Seq[Tree] = Nil
  }

}
