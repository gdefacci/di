package com.github.gdefacci.di.runtime

final case class Bind[A, B <: A]()

object Bind {
  
  def bind[T] = new Bind[T,T]()
}