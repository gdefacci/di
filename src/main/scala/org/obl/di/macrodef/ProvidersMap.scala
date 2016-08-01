package org.obl.di.macrodef

private[di] object ProvidersMap {
  
  private[ProvidersMap] def groupByMap[K,T](vs:Seq[(K,Dag[T])]):Map[K,Seq[Dag[T]]] =
    vs.groupBy(_._1).map( v => v._1 -> distinctValues(v._2.map(_._2)) )

  private[ProvidersMap] def distinctValues[T](dgs:Seq[Dag[T]]):Seq[Dag[T]] = {
    val b = collection.mutable.Buffer.empty[Dag[T]]
    val seen = collection.mutable.HashSet[T]()
    for (x <- dgs) {
      if (!seen(x.value)) {
        b += x
        seen += x.value
      }
    }
    b
  }
    
  def empty[KI,KT,T](reportDuplicateMapping:(KI,KT, Seq[Dag[T]]) => Nothing):ProvidersMap[KI,KT,T] = 
    new ProvidersMap[KI,KT,T]( Map.empty[(KI,KT), Seq[Dag[T]]], reportDuplicateMapping )
    
  def apply[KI,KT,T](mp:Seq[((KI, KT), Dag[T])], reportDuplicateMapping:(KI,KT, Seq[Dag[T]]) => Nothing):ProvidersMap[KI,KT,T] = 
    new ProvidersMap[KI,KT,T]( groupByMap[(KI,KT),T](mp), reportDuplicateMapping )
    
}

private[di] class ProvidersMap[KI, KT, T] private (val toMap: Map[(KI, KT), Seq[Dag[T]]], reportDuplicateMapping:(KI,KT, Seq[Dag[T]]) => Nothing) {

  import ProvidersMap.{groupByMap, distinctValues}
  
  def toSeq: Seq[((KI, KT), Dag[T])] = toMap.flatMap { e =>
    e._2.map { v =>
      e._1 -> v  
    }  
  }.toSeq

  def all(kid:KI, typPredicate:KT => Boolean): Seq[Dag[T]] = 
    toMap.flatMap {
      case ((id, t), sq) if id == kid && typPredicate(t) => sq
      case _ => Nil
    }.toSeq
    
    
  def get(k: (KI, KT)): Option[Dag[T]] = {
    toMap.get(k).flatMap {
      case Seq() => None
      case Seq(hd) => Some(hd)
      case dups => reportDuplicateMapping(k._1, k._2, dups)
    }
  }

  def +(e: ((KI, KT), Dag[T])): ProvidersMap[KI, KT, T] = this ++ Seq(e)
  def ++(e: Seq[((KI, KT), Dag[T])]): ProvidersMap[KI, KT, T] = this ++ groupByMap(e)
  def ++(e: ProvidersMap[KI, KT, T]): ProvidersMap[KI, KT, T] = this ++ e.toMap
  
  def ++ (e:Map[(KI, KT), Seq[Dag[T]]]):ProvidersMap[KI,KT,T] =
    new ProvidersMap[KI,KT,T]( e.foldLeft(toMap) { (mp, e) =>
      val nv = mp.get(e._1) match {
        case None => e._2
        case Some(vs) => vs ++ e._2
      }
      mp + (e._1 -> distinctValues(nv))
    }, reportDuplicateMapping )

  def values: Seq[Dag[T]] = toMap.values.flatten.toSeq

}