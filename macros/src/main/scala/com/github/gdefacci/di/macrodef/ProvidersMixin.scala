package com.github.gdefacci.di.macrodef

import scala.reflect.macros.blackbox.Context

trait ProvidersMixin[C <: Context] { self: DagNodes[C] with Unifier[C] =>
  val context: C

  import context.universe._

  type Providers[T] = ProvidersMap[T, DagNodeDagFactory, Ref, Type, Decorator, PolyDecorator]
}