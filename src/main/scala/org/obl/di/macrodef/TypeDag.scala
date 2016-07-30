package org.obl.di.macrodef

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

private[di] class TypeDag[C <: Context](val context: C) extends DagNodes[C] with DagNodeOrRefFactory[C] with DagNodeFactory[C] with DagToExpr[C] {

  import context.universe._
  
  type MappingEntry = ((Id, Symbol), Dag[DagNodeOrRef])

  def toDagNodesWithRefs(valueExpr: context.Expr[_], kindProvider: Symbol => Kinds): Map[(Id, Symbol), Dag[DagNodeOrRef]] = {
    val exprTyp = valueExpr.actualType
    val exprNm = TermName(context.freshName(exprTyp.typeSymbol.name.decodedName.toString))
    val exprDag = alias(exprNm, valueExpr.tree, Kind.default)

    val allMappings = ((exprDag.value.kind.id -> exprDag.value.typ.typeSymbol) -> exprDag) +: membersSelect.getBindings[Seq[MappingEntry]](exprTyp,
      { member =>
        methodDag(exprDag, exprNm, member, kindProvider).toSeq.flatMap {
          case dg @ Leaf(dn: DagNode) => (dn.kind.id -> dn.typ.typeSymbol) -> dg :: Nil
          case dg @ Node(dn: DagNode, _) => (dn.kind.id -> dn.typ.typeSymbol) -> dg :: Nil
          case _ => Nil
        }
      }, {
        case membersSelect.BindInstance(member, abstractType, concreteType) =>
          val knds = kindProvider(member)
          if (concreteType.isAbstract) {
            context.abort(member.pos, s"The second type parameter of Bind must be a concrete class, ${concreteType} is not")
          }
          val ref = Ref(knds, concreteType.info, member.pos)
          knds.ids.toSeq.map(id => (id -> abstractType) -> Leaf[DagNodeOrRef](ref))
      }).flatten

    checkNoDuplicates(allMappings)

    allMappings.toMap
  }
  
  def checkNoDuplicates[N <: DagNodeOrRef](dags: Seq[((Id, Symbol), Dag[N])]): Unit = {
    val dups = dags.groupBy( e => e._1).filter(_._2.length > 1)
    if (dups.nonEmpty) {
      val text =
        s"""
duplicates bindings:
${dups.map { dupEntry => s"""
type ${dupEntry._1} has more than one binding
${dupEntry._2.map { v => v._2 }.mkString("\n")}
"""}.mkString("")}
"""
      context.abort(context.enclosingPosition, text)
    }
  }

  def instantiateObject[T](knd: Kind,
                           typ: Type,
                           mappings: Map[(Id, Symbol), Dag[DagNodeOrRef]],
                           kindProvider: Symbol => Kinds): Expr[T] = {
    val dag = instantiateDag(knd, typ, mappings, kindProvider)
    
    val dag1 = Dag.update(dag) { (v, inps) =>
      val v1 = 
        if (v.kind.scope != SingletonScope) v
        else 
          DagNode(v.kind,
              s"singleton${v.description}}",
              inps => Seq(q"""
                val ${v.singletonName} = ${v.invoke(inps)}
                """),
              inps => q"${v.singletonName}",
              v.typ,
              v.sourcePos,
              "singleton")
      if (inps.isEmpty) Leaf(v1) else Node(v1, inps)
    }
    
    val dagExpr = dagToExpr(dag1)
    context.Expr[T](context.typecheck(dagExpr.toTree))
  }

}