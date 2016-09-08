package org.obl.di.macrodef

import collection.mutable.Buffer

class MapOfBuffers[K,V] private ( private val state:collection.mutable.Map[K, Buffer[V]] ) {
  
  def this() = this(collection.mutable.Map.empty[K, Buffer[V]])
  
  def values(k:K):Seq[V] = state.get(k).toSeq.flatten
  
  def values:Seq[V] = state.values.flatten.toSeq
  
  def += (k:K, v:V):Unit = this ++= (k, Seq(v))
  
  def ++= (vs:Seq[(K,V)]):Unit = vs.foreach { case (k,v) => this += (k,v) }
  
  def ++= (k:K, vs:Seq[V]):Unit = state.get(k) match {
    case None => state += (k -> vs.toBuffer)
    case Some(buff) => state += (k -> (buff ++ vs))
  }
  
  def ++= (othr:MapOfBuffers[K,V]):Unit = othr.state.foreach {
    case (k, vs) => this ++= (k,vs)
  }
  
  def replace(id:K, what:V => Boolean, withValue:V):Boolean = 
    state.get(id).map { buff =>
      var found = false
      buff.map {
        case x if what(x) => 
          found = true
          withValue
        case x => x
      }
      found
    }.getOrElse(false)
  
  def copy():MapOfBuffers[K,V] = {
    val mp1 = collection.mutable.Map.empty[K, Buffer[V]]
    mp1 ++= state
    new MapOfBuffers[K,V](mp1)
  }
  
  def ++ (othr:MapOfBuffers[K,V]):MapOfBuffers[K,V] = {
    val r = copy()
    r ++= othr
    r
  }
  
  def find(kid: K, typPredicate: V => Boolean): Seq[V] =
    state.get(kid).toSeq.flatMap { sq =>
      sq.filter(sg => typPredicate(sg))
    }
  
}