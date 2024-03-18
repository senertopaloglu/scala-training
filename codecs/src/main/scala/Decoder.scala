package com.jpmc.codecs

import com.jpmc.json.Json
import Decoder._
import com.jpmc.codecs.User

import java.time.LocalDate
trait Decoder[A] {
  def decode(value: Json): Result[A]

  def withError(msg: String): Decoder[A] = decode(_).left.map(_ => msg)
}


object Decoder {
  type Result[A] = Either[String, A]

  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  def from[A](f: Json => Result[A]): Decoder[A] = value => f(value)

  def fromPartial[A](f:PartialFunction[Json, Result[A]]): Decoder[A] = {
    val decoder = f.lift
    decoder(_) match {
      case Some(value) => value
      case None => Left("error decoding")
    }
  }

  implicit class DecoderOps(value: Json) {
    def as[A](implicit decoder: Decoder[A]): Result[A] = decoder.decode(value)
  }

  implicit class ObjDecoderOps(obj: Json.Object) {
    def decodeMap[A](key: java.lang.String)(implicit decoder: Decoder[A]): Result[A] = obj.data.get(key).toRight(s"unable to find value at key $key").flatMap(_.as[A])
  }

  def decode[A](input: Json)(implicit decoder: Decoder[A]): Result[A] = decoder.decode(input)

  implicit val intDecoder: Decoder[Int] = {
    Decoder.fromPartial {
      case Json.Number(value) => Right(value.toInt)
    }.withError("not an int")
  }

  implicit val booleanDecoder: Decoder[Boolean] = {
    Decoder.fromPartial {
      case Json.Boolean(value) => Right(value)
    }.withError("not a boolean")
  }

  implicit val stringDecoder: Decoder[String] = {
    Decoder.fromPartial {
      case Json.String(value) => Right(value)
    }.withError("not a string")
  }

  implicit val dateDecoder: Decoder[LocalDate] = {
    Decoder.fromPartial {
      case Json.String(value) => Right(LocalDate.parse(value))
    }.withError("not a date")
  }

  implicit def listDecoder[A](implicit decoder: Decoder[A]): Decoder[List[A]] = {
    fromPartial {
      case Json.Array(values) =>
        val listOfResults = values.map(decoder.decode)
        val listOfErrors = listOfResults.collect{case Left(value) => Left(value)}
        if (listOfErrors.isEmpty)
          Right(listOfResults.collect{case Right(value) => value})
        else
          Left(s"could not decode ${values.toString}")
    }
  }

  implicit def vectorDecoder[A](implicit decoder: Decoder[A]): Decoder[Vector[A]] = {
    listDecoder(decoder).withError("could not map list to vector").decode(_).map(_.toVector)
  }

  implicit def or[A,B](a: Decoder[A], b: Decoder[B]) = {
    from {
      json =>
        a.decode(json) match {
          case Left(value) => b.decode(json) match {
            case Left(value) => Left("could not decode")
            case Right(value) => Right(value)
          }
          case Right(value) => Right(value)
        }
    }
  }

  implicit def tuple2Decoder[A,B](implicit _1Decoder: Decoder[A], _2Decoder: Decoder[B]): Decoder[(A,B)] = {
    fromPartial {
      case obj: Json.Object => for {
        x <- obj.decodeMap[A]("_1")
        y <- obj.decodeMap[B]("_2")
      } yield(x,y)
    }.withError(s"could not decode tuple2")
  }

  implicit def tuple3Decoder[A, B, C](implicit _1Decoder: Decoder[A], _2Decoder: Decoder[B], _3Decoder: Decoder[C]): Decoder[(A, B, C)] = {
    fromPartial {
      case obj: Json.Object => for {
        x <- obj.decodeMap[A]("_1")
        y <- obj.decodeMap[B]("_2")
        z <- obj.decodeMap[C]("_3")
      } yield (x, y, z)
    }.withError(s"could not decode tuple3")
  }

  implicit def eitherDecoder[A: Decoder, B: Decoder]: Decoder[Either[A,B]] = {
    fromPartial {
      case Json.Object(m) if m.contains("left") =>
        m.get("left").toRight("Failed to decode left value").flatMap(_.as[A]).map(Left(_))
      case Json.Object(m) if m.contains("right") =>
        m.get("right").toRight("Failed to decode right value").flatMap(_.as[B]).map(Right(_))
    }
  }

  implicit def optionDecoder[A: Decoder](implicit decoder: Decoder[A]): Decoder[Option[A]] = {
    from {
      case Json.Null => Right(None)
      case json => decoder.decode(json) match {
        case Left(value) => Left(s"Cannot decode Some($value)")
        case Right(value) => Right(Some(value))
      }
    }
  }

  implicit val kindDecoder: Decoder[User.Kind] = fromPartial {
    case Json.String(value) => value match {
      case "Privileged" => Right(User.Kind.Privileged)
      case "Normal" => Right(User.Kind.Normal)
      case "Guest" => Right(User.Kind.Guest)
      case other => Left(s"Could not decode kind: $other")
    }
  }

  implicit val userDecoder: Decoder[User] = fromPartial {
    case o: Json.Object => for {
      name <- o.decodeMap[String]("name")
      age <- o.decodeMap[Int]("age")
      kind <- o.decodeMap[User.Kind]("kind")
    } yield User(name, age, kind)
  }.withError("Could not decode Json Object to User")

  

}


