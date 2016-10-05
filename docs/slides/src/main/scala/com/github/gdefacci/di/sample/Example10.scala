package com.github.gdefacci.di.sample

import com.github.gdefacci.di.IOC

object Example10 {

  val user = IOC.get[User](Module10Container)

  assert(user.admin == Module10C.isAdmin)
  assert(user.name == Module10B.name)
  assert(user.id == Module10A.anInt)


}