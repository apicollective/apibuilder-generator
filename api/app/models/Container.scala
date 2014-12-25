package models

import lib.Datatype

sealed trait Container

object Container {

  case object Singleton extends Container
  case object Option extends Container
  case object List extends Container
  case object Map extends Container

  def apply(dt: Datatype): Container = {
    dt match {
      case Datatype.Singleton(_) => Singleton
      case Datatype.Option(_) => Option
      case Datatype.List(_) => List
      case Datatype.Map(_) => Map
    }
  }

}

