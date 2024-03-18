package com.jpmc.codecs

import com.jpmc.codecs.User
import com.jpmc.json.Json
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.LocalDate

class DecoderTests extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {

  test("Decode string") {
    val str = "decode me"
    val jsonStr = Json.String(str)
    val expected = Right(str)
    val result = Decoder.DecoderOps(jsonStr).as[String]
    result should be(expected)
  }

  test("Fail to decode number as string") {
    val jsonNum = Json.Number(123)
    val expected = Left("not a string")
    val result = Decoder.DecoderOps(jsonNum).as[String]
    result should be(expected)
  }

  test("Decode number") {
    val num = 123
    val jsonNum = Json.Number(num)
    val expected = Right(num)
    val result = Decoder.DecoderOps(jsonNum).as[Int]
    result should be(expected)
  }

  test("Fail to decode string as number") {
    val jsonNum = Json.String("this is going to fail decoding")
    val expected = Left("not an int")
    val result = Decoder.DecoderOps(jsonNum).as[Int]
    result should be(expected)
  }

  test("Decode boolean") {
    val t = true
    val jsonBool = Json.Boolean(t)
    val expected = Right(t)
    val result = Decoder.DecoderOps(jsonBool).as[Boolean]
    result should be(expected)
  }

  test("Fail to decode number as boolean") {
    val jsonNum = Json.Number(123)
    val expected = Left("not a boolean")
    val result = Decoder.DecoderOps(jsonNum).as[Boolean]
    result should be(expected)
  }

  test("Decode date") {
    val dateStr = LocalDate.parse("2012-10-01")
    val date = Json.String("2012-10-01")
    val expected = Right(dateStr)
    val result = Decoder.DecoderOps(date).as[LocalDate]
    result should be(expected)
  }

  test("Fail to decode date") {
    val jsonNum = Json.Number(123)
    val expected = Left("not a date")
    val result = Decoder.DecoderOps(jsonNum).as[LocalDate]
    result should be(expected)
  }

  test("Decode tuple2") {
    val jsonObj = Json.Object(Map("_1" -> Json.String("value 1"), "_2" -> Json.String("value 2")))
    val expected = Right(("value 1", "value 2"))
    val result = Decoder[(String, String)].decode(jsonObj)
    result should be(expected)
  }

  test("Fail to decode tuple2") {
    val jsonObj = Json.Object(Map("_1" -> Json.String("value 1"), "_2" -> Json.Number(123)))
    val expected = Left("could not decode tuple2")
    val result = Decoder[(String, String)].decode(jsonObj)
    result should be(expected)
  }

  test("Decode tuple3") {
    val jsonObj = Json.Object(Map("_1" -> Json.String("value 1"), "_2" -> Json.String("value 2"), "_3" -> Json.Number(123)))
    val expected = Right(("value 1", "value 2", 123))
    val result = Decoder[(String, String, Int)].decode(jsonObj)
    result should be(expected)
  }

  test("Fail to decode tuple3") {
    val jsonObj = Json.Object(Map("_1" -> Json.String("a"), "_2" -> Json.Number(123), "_3" -> Json.String("b")))
    val expected = Left("could not decode tuple3")
    val result = Decoder[(String, String, String)].decode(jsonObj)
    result should be(expected)
  }

  test("Decode Either.Left") {
    val jsonObj = Json.Object(Map("left" -> Json.String("This is a Left")))
    val expected = Right(Left("This is a Left"))
    val result = Decoder[Either[String, String]].decode(jsonObj)
    result should be(expected)
  }

  test("Fail to decode Either.Left") {
    val jsonObj = Json.Object(Map("left" -> Json.Boolean(false)))
    val expected = Left("not a string")
    val result = Decoder[Either[String, String]].decode(jsonObj)
    result should be(expected)
  }

  test("Decode Either.Right") {
    val jsonObj = Json.Object(Map("right" -> Json.Number(123)))
    val expected = Right(Right(123))
    val result = Decoder[Either[String, Int]].decode(jsonObj)
    result should be(expected)
  }

  test("Fail to decode Either.Right") {
    val jsonObj = Json.Object(Map("right" -> Json.Boolean(true)))
    val expected = Left("not a string")
    val result = Decoder[Either[String, String]].decode(jsonObj)
    result should be(expected)
  }

  test("Decode Option.Some") {
    val json = Json.String("Hello Some")
    val expected = Right(Some("Hello Some"))
    val result = Decoder[Option[String]].decode(json)
    result should be(expected)
  }

  test("Decode Option.None") {
    val json = Json.Null
    val expected = Right(Option.empty[String])
    val result = Decoder[Option[String]].decode(json)
    result should be(expected)
  }

  test("Decode list") {
    val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2")))
    val expected = Right(List("entry 1", "entry 2"))
    val result = Decoder[List[String]].decode(jsonArr)
    result should be(expected)
  }

  test("Fail to decode list") {
    val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2"), Json.Number(1)))
    val expected = Left("could not decode List(String(entry 1), String(entry 2), Number(1))")
    val result = Decoder[List[String]].decode(jsonArr)
    result should be(expected)
  }

  test("Decode vector") {
    val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2")))
    val expected = Right(Vector("entry 1", "entry 2"))
    val result = Decoder[Vector[String]].decode(jsonArr)
    result should be(expected)
  }

  test("Fail to decode vector") {
    val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2"), Json.Number(1)))
    val expected = Left("could not map list to vector")
    val result = Decoder[Vector[String]].decode(jsonArr)
    result should be(expected)
  }

  test("Object decoder ops: successfully decode string") {
    val obj = Json.Object(Map("x" -> Json.String("a"), "y" -> Json.String("b")))
    val expected = Right("a")
    val result = Decoder.ObjDecoderOps(obj).decodeMap[String]("x")
    result should be(expected)
  }

  test("Map decoder ops: fail to find key") {
    val key = "z"
    val obj = Json.Object(Map("x" -> Json.String("a"), "y" -> Json.String("b")))
    val expected = Left(s"unable to find value at key $key")
    val result = Decoder.ObjDecoderOps(obj).decodeMap[String](key)
    result should be(expected)
  }

  test("Decode User.Kind") {
    val str = Json.String("Privileged")
    val expected = Right(User.Kind.Privileged)
    val result = Decoder.DecoderOps(str).as[User.Kind]
    result should be(expected)
  }

  test("Fail to decode User.Kind that doesn't exist") {
    val str = Json.String("Other")
    val expected = Left("Could not decode kind: Other")
    val result = Decoder[User.Kind].decode(str)
    result should be(expected)
  }

  test("Decode user") {
    val jsonObj = Json.Object(Map("name" -> Json.String("Mon"), "age" -> Json.Number(24), "kind" -> Json.String("Normal")))
    val expected = Right(User("Mon", 24, User.Kind.Normal))
    val result = Decoder[User].decode(jsonObj)
    result should be(expected)
  }

  test("Fail to decode user") {
    val str = Json.String("This is not a user.")
    val expected = Left("Could not decode Json Object to User")
    val result = Decoder[User].decode(str)
    result should be(expected)
  }








}
