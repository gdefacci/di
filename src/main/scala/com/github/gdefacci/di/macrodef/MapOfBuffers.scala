package com.github.gdefacci.di.macrodef

import collection.mutable.Buffer

object MapOfBuffers {
  def empty[K, V] = new MapOfBuffers[K, V](collection.mutable.Map.empty)
}

class MapOfBuffers[K, V] private (private val state: collection.mutable.Map[K, Buffer[V]]) {

  def values(k: K): Seq[V] = state.get(k).toSeq.flatten

  def values: Seq[V] = state.values.flatten.toSeq

  def +=(k: K, v: V): Unit = this ++= (k, Seq(v))

  def ++=(vs: Seq[(K, V)]): Unit = vs.foreach { case (k, v) => this += (k, v) }

  private def ++=(k: K, vs: Seq[V]): Unit = state.get(k) match {
    case None => state += (k -> vs.toBuffer)
    case Some(buff) => state += (k -> (buff ++ vs))
  }

  def ++=(othr: MapOfBuffers[K, V]): Unit = othr.state.foreach {
    case (k, vs) => this ++= (k, vs)
  }

  def copy(): MapOfBuffers[K, V] = {
    val mp1 = collection.mutable.Map.empty[K, Buffer[V]]
    mp1 ++= state.map { case (k, bf) => k -> bf.clone() }
    new MapOfBuffers[K, V](mp1)
  }

  def find(kid: K, typPredicate: V => Boolean): Seq[V] =
    state.get(kid).toSeq.flatMap { sq =>
      sq.filter(sg => typPredicate(sg))
    }

}