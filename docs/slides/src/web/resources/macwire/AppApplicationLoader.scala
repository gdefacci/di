package com.softwaremill.play24

import ...

trait AppComponents
extends BuiltInComponents
with NingWSComponents // for wsClient
with DatabaseModule // Database injection
with DaoModule
with ControllerModule // Application controllers
{
  lazy val assets: Assets = wire[Assets]
  lazy val router: Router = {
    lazy val prefix = "/"
    wire[Routes]
  }

  // The seed method is here just for demonstration purposes. Ideally this will be run in a task.
  def coffeeDao: CoffeeDao
  def supplierDao: SupplierDao
  val seed = wire[Seed]
  seed.run()
}

