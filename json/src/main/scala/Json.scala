package com.jpmc.json

sealed abstract class Json extends Product with Serializable

object Json {
  case class Object(data: Map[java.lang.String, Json]) extends Json

  case class Array(data: List[Json]) extends Json

  case class String(value: java.lang.String) extends Json

  case class Number(value: BigDecimal) extends Json

  case class Boolean(value: scala.Boolean) extends Json

  case object Null extends Json

  def apply(value: java.lang.String): Json = String(value)

  def apply(value: BigDecimal): Json = Number(value)

  def apply(value: scala.Boolean): Json = Boolean(value)

  def apply(value: List[Json]): Json = Array(value)

  def apply(value: Map[java.lang.String, Json]): Json = Object(value)

  def prettyPrintVal(input: Json): java.lang.String = {
    input match {
      case String(value) => s"""\"$value\""""
      case Number(value) => value.toString
      case Boolean(value) => value.toString
      case Null => "null"
      case Array(values) => prettyPrintDocument(Array(values))
      case Object(values) => prettyPrintDocument(simplifyNull(Object(values)))
    }
  }

  def prettyPrintDocument(document: Json): java.lang.String = {
    document match {
      case Array(values) => values.map(prettyPrintVal).mkString("[", ",", "]")
      case Object(pairs) => pairs.map {
        case (key, value) => s""""$key":${prettyPrintVal(value)}"""
      }.mkString("{", ",", "}")
      case _ => prettyPrintVal(document)
    }
  }

  def simplifyNull(input: Object): Object = {
    //input.data.filter((name, value) => value != null)
    Object(input.data.filter(_._2 != Null))
  }


  def main(args: scala.Array[java.lang.String]): Unit = {
    println(prettyPrintVal(simplifyNull(Object(Map[java.lang.String, Json]("name" -> String("John Doe"), "age" -> Null)))))
  }



}