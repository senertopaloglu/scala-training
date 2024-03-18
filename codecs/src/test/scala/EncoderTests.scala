package com.jpmc.codecs

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.jpmc.json.Json

import java.time.LocalDate

class EncoderTests extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {
  test("Boolean Encoder") {
    val booleanTrue = true
    val result = Encoder.encode(booleanTrue)
    val expected = Json.Boolean(true)
    result should be(expected)
  }

  test("String Encoder") {
    val stringExample = "John Doe"
    val result = Encoder.encode(stringExample)
    val expected = Json.String("John Doe")
    result should be(expected)
  }

  test("Integer Encoder") {
    val numberExample = 10
    val result = Encoder.encode(numberExample)
    val expected = Json.Number(10)
    result should be(expected)
  }

  test("Date Encoder") {
    val dateExample = LocalDate.of(2012, 10, 1)
    val result = Encoder.encode(dateExample)
    val expected = Json.String("2012-10-01")
    result should be(expected)
  }

  test("Tuple2 Encoder") {
    val example: Tuple2[String, Int] = ("John Doe", 100)
    val result = Encoder.encode(example)
    val expected =
      Json.Object(
        Map(
          "_1" -> Json.String("John Doe"),
          "_2" -> Json.Number(100)
        )
      )
    result should be(expected)
  }

  test("Tuple3 Encoder") {
    val example: Tuple3[String, Int, Boolean] = ("John Doe", 100, false)
    val result = Encoder.encode(example)
    val expected =
      Json.Object(
        Map(
          "_1" -> Json.String("John Doe"),
          "_2" -> Json.Number(100),
          "_3" -> Json.Boolean(false)
        )
      )
    result should be(expected)
  }

  test("List Encoder") {
    val example = List("A", "B", "C", "D")
    val result = Encoder.encode(example)
    val expected = Json.Array(List(Json.String("A"), Json.String("B"), Json.String("C"),Json.String("D")))
    result should be(expected)
  }

  test("Vector Encoder") {
    val example = Vector("A", "B", "C", "D")
    val result = Encoder.encode(example)
    val expected = Json.Array(List(Json.String("A"), Json.String("B"), Json.String("C"), Json.String("D")))
    result should be(expected)
  }

  test("Option Encoder - Some") {
    val example: Option[Int] = Some(10)
    val result = Encoder.encode(example)
    val expected = Json.Number(10)
    result should be(expected)
  }

  test("Option Encoder - None") {
    val example: Option[Int] = None
    val result = Encoder.encode(example)
    val expected = Json.Null
    result should be(expected)
  }

  test("Either Encoder - Int, Int") {
    val example: Either[Int, Int] = Left(4)
    val result = Encoder.encode(example)
    val expected = Json.Number(4)
    result should be(expected)
  }

  test("User Encoder") {
    val example: User = User("john doe", 21, User.Kind.Normal)
    val result = Encoder.encode(example)
    val expected = {
      Json.Object(
        Map(
          "name" -> Json.String("john doe"),
          "age" -> Json.Number(21),
          "kind" -> Json.String("Normal")
        )
      )
    }
    result should be(expected)
  }


}
