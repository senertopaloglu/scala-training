package com.jpmc.codecs

import com.jpmc.json.Json
import com.jpmc.codecs.User

import java.time.LocalDate

trait Encoder[A] {
  def encode(value: A): Json
}

object Encoder {

  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  def from[A](f: A=>Json):Encoder[A] =
    new Encoder[A] {
      override def encode(value: A): Json = f(value)
    }

  def encode[A](input: A)(implicit encoder: Encoder[A]): Json = encoder.encode(input)

  implicit class EncoderOps[A](a: A) {
    def asJson(implicit encoder: Encoder[A]): Json = encoder.encode(a)
  }

  implicit val booleanEncoder: Encoder[Boolean] = Json.Boolean(_)

  implicit val stringEncoder: Encoder[java.lang.String] = Json.String(_)

  implicit val intEncoder: Encoder[Int] = Json.Number(_)

  implicit val dateEncoder: Encoder[LocalDate] = date => Json.String(date.toString)

  implicit def tuple2Encoder[A,B](implicit encoder1: Encoder[A], encoder2: Encoder[B]): Encoder[Tuple2[A,B]] = t2 =>
    Json.Object(Map("_1" -> encoder1.encode(t2._1), "_2" -> encoder2.encode(t2._2)))

  implicit def tuple3Encoder[A, B, C](implicit e1: Encoder[A], e2: Encoder[B], e3: Encoder[C]): Encoder[Tuple3[A, B, C]] = t3 =>
    Json.Object(Map("_1" -> e1.encode(t3._1), "_2" -> e2.encode(t3._2), "_3" -> e3.encode(t3._3)))

  implicit def listEncoder[A](implicit encoder: Encoder[A]): Encoder[List[A]] = values =>
    Json.Array(values.map(encoder.encode))

  implicit def vectorEncoder[A](implicit encoder: Encoder[A]): Encoder[Vector[A]] = values =>
    Json.Array(values.map(encoder.encode).toList)

  implicit def optionEncoder[A](implicit encoder: Encoder[A]): Encoder[Option[A]] = value =>
    value match {
      case Some(x) => encoder.encode(x)
      case None => Json.Null
    }

  implicit def eitherEncoder[A, B](implicit leftEncoder: Encoder[A], rightEncoder: Encoder[B]): Encoder[Either[A, B]] = value =>
    value match {
      case Left(value) => leftEncoder.encode(value)
      case Right(value) => rightEncoder.encode(value)
    }

  implicit val kindEncoder: Encoder[User.Kind] = from {
    case User.Kind.Normal => Json.String("Normal")
    case User.Kind.Privileged => Json.String("Privileged")
    case User.Kind.Guest => Json.String("Guest")
  }

  implicit val userEncoder: Encoder[User] =
    from {
      user => Json.Object(
        Map(
          "name" -> user.name.asJson,
          "age" -> user.age.asJson,
          "kind" -> user.kind.asJson
        )
      )
    }

}