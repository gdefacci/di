package org.obl.di.macrodef

import scala.reflect.macros.blackbox.Context

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
    def description:String
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
      apply(kind, s"$method", trees => Nil, inputs => reflectUtils.methodCall(containerTermName, methodSymbol, inputs), methodSymbol.returnType, method.pos, method.name.decodedName.toString)    }

    def constructorCall(kind: Kind, containerTermName: Option[TermName], typ: Type, constructor: MethodSymbol, members: Seq[Tree]):DagNode = {
      val invoker: Seq[Tree] => Tree = { inputs => 
        if (members.isEmpty) reflectUtils.methodCall(containerTermName, constructor, inputs)
        else reflectUtils.newAbstractClass(constructor.owner, constructor.paramLists, inputs, members)
      }
      apply(kind, s"$constructor", trees => Nil, invoker, typ, constructor.pos, typ.typeSymbol.name.decodedName.toString)
    }
    
  }
  
  private def reportDuplicateMapping[T](id:Id, typeSymbol:Symbol, dags:Seq[Dag[T]]):Nothing = {
             val text =
        s"""
duplicates bindings: ${dags.length}
type ${typeSymbol} ${id} has more than one binding
${dags.map { dupEntry => s"""
${dupEntry}
"""}.mkString("")}
"""
      context.abort(context.enclosingPosition, text)
  }  
  
  type Providers[T] = ProvidersMap[Id, Symbol, T]

  object Providers {
    
    def empty[T]:Providers[T] = ProvidersMap.empty[Id, Symbol, T]( reportDuplicateMapping )
    def apply[T](entries:Seq[((Id, Symbol), Dag[T])]):Providers[T] = ProvidersMap[Id, Symbol,T](entries, reportDuplicateMapping)
    
  }
}
