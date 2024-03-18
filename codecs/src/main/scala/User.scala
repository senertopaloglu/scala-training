package com.jpmc.codecs

case class User(name: String, age: Int, kind: User.Kind)

object User {
  sealed trait Kind
  object Kind {
    case object Privileged extends Kind
    case object Normal extends Kind
    case object Guest extends Kind
  }
}