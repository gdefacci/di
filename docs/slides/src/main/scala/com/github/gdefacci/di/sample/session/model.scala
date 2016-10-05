package com.github.gdefacci.di.sample.session

case class User(userName:String, password:String)
case class Session(user:User, appData:AppData)
case class AppData(total:Int)

case class MyService(repo:Repository) {
  def doIt(a:Int) = "done"
}
case class Repository(conn:Connection)
case class Connection(user:User)
