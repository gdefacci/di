package org.obl.di.macrodef

private[di] object ProvidersMap {
  
 private[ProvidersMap] def groupByMap[K,T](vs:Seq[(K,Dag[T])]):Map[K,Seq[Dag[T]]] =
    vs.groupBy(_._1).map( v => v._1 -> distinctValues(v._2.map(_._2), (dag:Dag[T]) => dag.value))
  
  private[ProvidersMap] def distinctValues[T,K](dgs:Seq[T], selectKey:T => K):Seq[T] = {
    val b = collection.mutable.Buffer.empty[T]
    val seen = collection.mutable.HashSet[K]()
    for (x <- dgs) {
      val key = selectKey(x)
      if (!seen(key)) {
        b += x
        seen += key
      }
    }
    b
  }

  
  def empty[KI,T, TF]:ProvidersMap[KI,T, TF] = 
    new ProvidersMap[KI,T, TF]( Map.empty, Map.empty )
    
  def apply[KI,T, TF](mp:Seq[(KI, Dag[T])], polyMembersMap: Seq[(KI, TF)]):ProvidersMap[KI,T,TF] = 
    new ProvidersMap[KI,T, TF]( groupByMap[KI,T](mp), polyMembersMap.groupBy(_._1).map {
      case (ki, ps) => ki -> ps.map(_._2)
    } )
    
}

private[di] class ProvidersMap[KI, T, TF] private (private val membersMap: Map[KI, Seq[Dag[T]]], private val polyMembersMap: Map[KI, Seq[TF]]) {

  import ProvidersMap.{groupByMap, distinctValues}
  
  def findMembers(kid:KI, typPredicate:(T) => Boolean): Seq[Dag[T]] = 
    membersMap.get(kid).toSeq.flatMap { sq =>
      sq.filter( sg => typPredicate(sg.value))
    }
  
  def findPolymorphicMembers[R](kid:KI, select:TF => Option[R]): Seq[R] = 
    polyMembersMap.get(kid).toSeq.flatMap { sq =>
      sq.map(select).collect { case Some(v) => v }
    }
    
  def +(e: (KI, Dag[T])): ProvidersMap[KI, T, TF] = this ++ Seq(e)
  def ++(e: Seq[(KI, Dag[T])]): ProvidersMap[KI, T, TF] = this ++ groupByMap(e)
  def ++(e: ProvidersMap[KI, T, TF]): ProvidersMap[KI, T, TF] = {
   new ProvidersMap[KI,T, TF]( e.membersMap.foldLeft(membersMap) { (mp, e) =>
      val nv = mp.get(e._1) match {
        case None => e._2
        case Some(vs) => vs ++ e._2
      }
      mp + (e._1 -> distinctValues(nv, (dag:Dag[T]) => dag.value))
    }, polyMembersMap ++ e.polyMembersMap)
  }
  
  private def ++ (e:Map[KI, Seq[Dag[T]]]):ProvidersMap[KI,T, TF] =
    new ProvidersMap[KI,T, TF]( e.foldLeft(membersMap) { (mp, e) =>
      val nv = mp.get(e._1) match {
        case None => e._2
        case Some(vs) => vs ++ e._2
      }
      mp + (e._1 -> distinctValues(nv, (dag:Dag[T]) => dag.value))
    }, polyMembersMap)

  def members: Seq[Dag[T]] = membersMap.values.flatten.toSeq
  def polyMembers: Seq[TF] = polyMembersMap.values.flatten.toSeq
  
}