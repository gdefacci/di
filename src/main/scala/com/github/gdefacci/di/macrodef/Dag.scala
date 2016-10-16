package com.github.gdefacci.di.macrodef

private[di] sealed trait Dag[+T] {

  def value: T
  def inputs: Seq[Dag[T]]

}

private[di] final case class Leaf[T](value: T) extends Dag[T] {
  val inputs: Seq[Dag[T]] = Nil
}
private[di] final case class Node[T](value: T, inputs: Seq[Dag[T]]) extends Dag[T]

import collection.mutable.{ Set => MSet, Map => MMap }

private[di] object Dag {

  def update[T](dag: Dag[T])(f: (T, Seq[Dag[T]]) => Dag[T]): Dag[T] = {
    mapValues[T, T, Dag[T]](dag, dg => dg)((nd, inps) => f(nd.value, inps))
  }

  def visit[T](d: Dag[T]): Seq[Dag[T]] = {
    foldDistinct[T,T, Seq[Dag[T]]](d, d => d, Nil)( (acc, d)=> acc :+ d )
  }
  
  def fold[T, ACC](dag:Dag[T], zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    val z1 = dag.inputs.foldLeft(zero) { (acc, i) =>
      fold(i, acc)(f)
    }
    f(z1, dag)
  }
  
  def mapValues[A, ID, B](d:Dag[A], keyf:A => ID)(f:(Dag[A], Seq[B]) => B) = {
    val mp = fold[A, Map[ID, B]](d, Map.empty) { (acc, dg) =>
      val k = keyf(dg.value)
      acc.get(k) match {
        case Some(_) => acc
        case None =>
          val v = f(dg, dg.inputs.map { dg =>
            acc(keyf(dg.value))
          })
          acc + (k -> v)
      }
    }
    mp(keyf(d.value))
  }
  
  def foldDistinctPair[T, ID, ACC](dag:Dag[T], keyf:T => ID, zero:ACC, initial:Set[ID])(f:(ACC, Dag[T]) => ACC) :(Set[ID], ACC) = {
    fold(dag, Set.empty[ID] -> zero) { (acc0, i) => 
      val (visited, acc) = acc0
      val k= keyf(i.value)
      if (visited.contains(k)) acc0
      else (visited + k) -> f(acc, i)        
    }
  }

  
  def foldDistinct[T, ID, ACC](dag:Dag[T], keyf:T => ID, zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    foldDistinctPair[T, ID, ACC](dag, keyf, zero, Set.empty)(f)._2
  }
  
  def foldDistinctSeq[T, ID, ACC](dags:Seq[Dag[T]], keyf:T => ID, zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    val z = Set.empty[ID]
    dags.foldLeft(z -> zero) { (acc, dag) =>
      val (ids, res) = acc
      foldDistinctPair[T, ID, ACC](dag, keyf, res, ids)(f)
    }._2
  }

}

class DagConnections[T](pred: T => Boolean, current: MMap[Dag[T], Boolean] = MMap.empty[Dag[T], Boolean]) {
  
  def isConnected(dag: Dag[T]): Boolean = {
    current.get(dag) match {
      case Some(v) => v
      case None =>
        val r =
          if (pred(dag.value)) true
          else dag.inputs.exists(inp => isConnected(inp))
        current += (dag -> r)
        r
    }
  }
  
}

class NDagConnections[T, ID](pred: T => Boolean, keyf:T => ID, current: MMap[ID, Boolean] = MMap.empty[ID, Boolean]) {
  
  def isConnected(dag: Dag[T]): Boolean = {
    val key = keyf(dag.value)
    current.get(key) match {
      case Some(v) => v
      case None =>
        val r =
          if (pred(dag.value)) true
          else dag.inputs.exists(inp => isConnected(inp))
        current += (key -> r)
        r
    }
  }
  
}


