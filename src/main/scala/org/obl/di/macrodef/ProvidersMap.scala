package org.obl.di.macrodef

private[di] object ProvidersMap {

  private[ProvidersMap] def groupByMap[K, T](vs: Seq[(K, Dag[T])]): Map[K, Seq[Dag[T]]] =
    vs.groupBy(_._1).map(v => v._1 -> distinctValues(v._2.map(_._2), (dag: Dag[T]) => dag.value))

  private[ProvidersMap] def distinctValues[T, K](dgs: Seq[T], selectKey: T => K): Seq[T] = {
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

  def empty[KI, T, TF]: ProvidersMap[KI, T, TF] =
    new ProvidersMap[KI, T, TF](Map.empty, Map.empty)

  def apply[KI, T, TF](mp: Seq[(KI, Dag[T])], polyMembersMap: Seq[(KI, TF)]): ProvidersMap[KI, T, TF] =
    new ProvidersMap[KI, T, TF](groupByMap[KI, T](mp), polyMembersMap.groupBy(_._1).map {
      case (ki, ps) => ki -> ps.map(_._2)
    })

}

private[di] class ProvidersMap[KI, T, TF] private (private[di] val membersMap: Map[KI, Seq[Dag[T]]], private[di] val polyMembersMap: Map[KI, Seq[TF]]) {

  import ProvidersMap.{ groupByMap, distinctValues }

  def findMembers(kid: KI, typPredicate: (T) => Boolean): Seq[Dag[T]] =
    membersMap.get(kid).toSeq.flatMap { sq =>
      sq.filter(sg => typPredicate(sg.value))
    }

  def findPolymorphicMembers[R](kid: KI, select: TF => Option[R]): Seq[R] =
    polyMembersMap.get(kid).toSeq.flatMap { sq =>
      sq.map(select).collect { case Some(v) => v }
    }

  def collectValues[T1](pred: PartialFunction[(KI, Dag[T]), Dag[T1]]): ProvidersMap[KI, T1, TF] =
    new ProvidersMap[KI, T1, TF](
      (membersMap.map {
        case (id, dgs) => id -> (dgs.flatMap { dg =>
          if (pred.isDefinedAt(id -> dg)) pred(id -> dg) :: Nil
          else Nil
        })
      }).toMap.filter(_._2.nonEmpty), polyMembersMap)

  def +(e: (KI, Dag[T])): ProvidersMap[KI, T, TF] = this ++ Seq(e)
  def ++(e: Seq[(KI, Dag[T])]): ProvidersMap[KI, T, TF] = add(groupByMap(e), Map.empty)
  def ++(e: ProvidersMap[KI, T, TF]): ProvidersMap[KI, T, TF] = add(e.membersMap, e.polyMembersMap)

  private def add(othrMembersMap: Map[KI, Seq[Dag[T]]], othrPolyMembersMap: Map[KI, Seq[TF]]) = {
    new ProvidersMap[KI, T, TF](othrMembersMap.foldLeft(membersMap) { (mp, e) =>
      val nv = mp.get(e._1) match {
        case None => e._2
        case Some(vs) => vs ++ e._2
      }
      mp + (e._1 -> distinctValues(nv, (dag: Dag[T]) => dag.value))
    }, polyMembersMap ++ othrPolyMembersMap)
  }

  def members: Seq[Dag[T]] = membersMap.values.flatten.toSeq
  def polyMembers: Seq[TF] = polyMembersMap.values.flatten.toSeq

  //  def toMProvidersMap: Unit = new MProvidersMap[KI,T,TF](e.membersMap, e.polyMembersMap)

}

object MProvidersMap {

  def empty[KI, T, TF] = new MProvidersMap[KI, T, TF]

  def apply[KI, T, TF](mp: Seq[(KI, Dag[T])], polyMembersMap: Seq[(KI, TF)]): MProvidersMap[KI, T, TF] = {
    val res = empty[KI, T, TF]

    mp.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res += (k, _))
    }
    polyMembersMap.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
      case (k, vs) => vs.foreach(res.poly += (k, _))
    }
    res
  }

}
private[di] class MProvidersMap[KI, T, TF] private ( 
    private val membersMap:MapOfBuffers[KI, Dag[T]],
    private val polyMembersMap:MapOfBuffers[KI, TF]) {
  
  def this() = this(new MapOfBuffers[KI, Dag[T]], new MapOfBuffers[KI, TF])
  
  def findMembers(kid: KI, typPredicate: (T) => Boolean): Seq[Dag[T]] = membersMap.find(kid, (dag) => typPredicate(dag.value))
  def findPolymorphicMembers[R](kid: KI, select: TF => Option[R]): Seq[R] = 
    polyMembersMap.values(kid).map(select).collect { case Some(v) => v }
  
  def +=(e: (KI, Dag[T])): Unit = membersMap += (e._1, e._2)
  def ++=(mp: Seq[(KI, Dag[T])]): Unit = membersMap ++= (mp)
    
  object poly {
    def +=(e: (KI, TF)): Unit = polyMembersMap += (e._1, e._2)
    def ++=(mp: Seq[(KI, TF)]): Unit = polyMembersMap ++= mp
  }
  
  def ++=(e: MProvidersMap[KI, T, TF]): Unit = {
    membersMap ++= e.membersMap
    polyMembersMap ++= e.polyMembersMap
  }
  
  def replaceMember(id:KI, what:Dag[T] => Boolean, withDag:Dag[T]) = membersMap.replace(id, what, withDag)
  
  def members: Seq[Dag[T]] = membersMap.values
  def polyMembers: Seq[TF] = polyMembersMap.values
    
  def copy() = new MProvidersMap(membersMap.copy(), polyMembersMap.copy())
}

//private[di] class MProvidersMap[KI, T, TF] private (
//    private val membersMap: collection.mutable.Map[KI, collection.mutable.Buffer[Dag[T]]],
//    private val polyMembersMap: collection.mutable.Map[KI, collection.mutable.Buffer[TF]]) {
//
//  import ProvidersMap.{ groupByMap, distinctValues }
//
//  def findMembers(kid: KI, typPredicate: (T) => Boolean): Seq[Dag[T]] =
//    membersMap.get(kid).toSeq.flatMap { sq =>
//      sq.filter(sg => typPredicate(sg.value))
//    }
//
//  def findPolymorphicMembers[R](kid: KI, select: TF => Option[R]): Seq[R] =
//    polyMembersMap.get(kid).toSeq.flatMap { sq =>
//      sq.map(select).collect { case Some(v) => v }
//    }
//
////  def collectValues[T1](pred: PartialFunction[(KI, Dag[T]), Dag[T1]]): MProvidersMap[KI, T1, TF] =
////    new MProvidersMap[KI, T1, TF](
////      (membersMap.map {
////        case (id, dgs) => id -> (dgs.flatMap { dg =>
////          if (pred.isDefinedAt(id -> dg)) pred(id -> dg) :: Nil
////          else Nil
////        })
////      }).filter(_._2.nonEmpty), polyMembersMap)
//
//  def +=(e: (KI, Dag[T])): Unit = this ++= Seq(e)
//  def ++=(mp: Seq[(KI, Dag[T])]): Unit =
//    mp.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
//      case (k, vs) => membersMap.get(k) match {
//        case None => membersMap += (k -> vs.toBuffer)
//        case Some(tvs) => tvs ++= vs
//      }
//    }
//  //    e.foreach {
//  //    case (k,v) => membersMap.get(k) match {
//  //      case None => membersMap += (k -> collection.mutable.Buffer(v))
//  //      case Some(vs) => vs += v
//  //    }
//  //  }
//
//  def ++=(e: MProvidersMap[KI, T, TF]): Unit = add(e.membersMap, e.polyMembersMap)
//
//  object poly {
//    def +=(e: (KI, TF)): Unit = this ++= Seq(e)
//    def ++=(mp: Seq[(KI, TF)]): Unit =
//      mp.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.foreach {
//        case (k, vs) => polyMembersMap.get(k) match {
//          case None => polyMembersMap += (k -> vs.toBuffer)
//          case Some(tvs) => tvs ++= vs
//        }
//      }
//  }
//  //      e.foreach {
//  //      case (k,v) => polyMembersMap.get(k) match {
//  //        case None => polyMembersMap += (k -> collection.mutable.Buffer(v))
//  //        case Some(vs) => vs += v
//  //      }
//  //    }
//  //  }
//
//  private def add(othrMembersMap: collection.mutable.Map[KI, collection.mutable.Buffer[Dag[T]]], othrPolyMembersMap: collection.mutable.Map[KI, collection.mutable.Buffer[TF]]): Unit = {
//    othrMembersMap.foreach {
//      case (k,vs) => vs.foreach( this += (k, _) )
//    }
//    polyMembersMap.foreach {
//      case (k,vs) => vs.foreach( this.poly += (k, _) )
//    }
//    
//    
////    membersMap.foreach {
////      case (k, vs) => othrMembersMap.get(k).foreach(vs1 => vs ++= vs1)
////    }
////    polyMembersMap.foreach {
////      case (k, vs) => othrPolyMembersMap.get(k).foreach(vs1 => vs ++= vs1)
////    }
//  }
//
//  def members: Seq[Dag[T]] = membersMap.values.flatten.toSeq
//  def polyMembers: Seq[TF] = polyMembersMap.values.flatten.toSeq
//
//}