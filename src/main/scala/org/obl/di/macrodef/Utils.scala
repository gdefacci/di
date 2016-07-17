package org.obl.di.macrodef

private[di] object Utils {

  def byLenghtsPartition[T](lenghts: Seq[Int], arguments: List[T]): List[List[T]] = {
    assert(lenghts.sum == arguments.length, s"wrong number of arguments")
    lenghts match {
      case Nil => Nil
      case hd :: Nil =>
        assert(hd == arguments.length)
        List(arguments)
      case hd +: rest =>
        val (curr, restArgs) = arguments.splitAt(hd)
        curr +: byLenghtsPartition(rest, restArgs)
    }
  }
  
}
