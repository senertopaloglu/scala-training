package com.jpmc.json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.jpmc.json.Json

class JsonTests extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {

  import TestHelper._

  test("prettyPrint Json Array") {
    val result = Json.prettyPrintDocument(exampleArray)
    val expected =
      """["Apples",true]"""
    result should be(expected)
  }

  test("prettyPrint Json Object") {
    val result = Json.prettyPrintDocument(exampleObject)
    val expected =
      """{"name":"John Doe","age":30,"profession":"Taxi Driver"}"""
    result should be(expected)
  }
}

object TestHelper {
  val exampleArray: Json =
    Json(
      List(
        Json.String("Apples"), Json.Boolean(true)
      )
    )

  val exampleObject: Json =
    Json(
      Map(
        ("name", Json.String("John Doe")),
        ("age", Json.Number(30)),
        ("profession", Json.String("Taxi Driver"))
      )
    )
}
