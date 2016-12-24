package com.github.gdefacci.di.macrodef

private[di] final case class Dag[+T](value: T, inputs: Seq[Dag[T]] = Nil) {
  def fold[ACC](zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    val z1 = inputs.foldLeft(zero) { (acc, i) =>
      i.fold(acc)(f)
    }
    f(z1, this)
  }
  def visit: Seq[Dag[T]] = {
    foldDistinct[T, Seq[Dag[T]]](d => d, Nil)( (acc, d)=> acc :+ d )
  }
  def foldDistinctPair[ID, ACC](keyf:T => ID, zero:ACC, initial:Set[ID])(f:(ACC, Dag[T]) => ACC) :(Set[ID], ACC) = {
    fold(Set.empty[ID] -> zero) { (acc0, i) => 
      val (visited, acc) = acc0
      val k= keyf(i.value)
      if (visited.contains(k)) acc0
      else (visited + k) -> f(acc, i)        
    }
  }
  def foldDistinct[ID, ACC](keyf:T => ID, zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    foldDistinctPair[ID, ACC](keyf, zero, Set.empty)(f)._2
  }
  
}

import collection.mutable.{ Set => MSet, Map => MMap }

private[di] object Dag {
  
  def update[T](dag: Dag[T])(f: (T, Seq[Dag[T]]) => Dag[T]): Dag[T] = {
    mapValues[T, T, Dag[T]](dag, dg => dg)((nd, inps) => f(nd.value, inps))
  }

  def mapValues[A, ID, B](d:Dag[A], keyf:A => ID)(f:(Dag[A], Seq[B]) => B) = {
    val mp = d.fold[Map[ID, B]](Map.empty) { (acc, dg) =>
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
  
  def foldDistinctSeq[T, ID, ACC](dags:Seq[Dag[T]], keyf:T => ID, zero:ACC)(f:(ACC, Dag[T]) => ACC) :ACC= {
    val z = Set.empty[ID]
    dags.foldLeft(z -> zero) { (acc, dag) =>
      val (ids, res) = acc
      dag.foldDistinctPair[ID, ACC](keyf, res, ids)(f)
    }._2
  }

}

private[di] class DagConnections[T, ID](pred: T => Boolean, keyf:T => ID, current: MMap[ID, Boolean] = MMap.empty[ID, Boolean]) {
  
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


