package com.identityblitz.json


import org.scalatest.{Matchers, FlatSpec}

class JsonTest extends FlatSpec with Matchers  {

  behavior of "Checking of JSON serialization"

  it should "serialization result must be equal to '{\"key1\":12,\"key2\":\"value2\",\"key3\":true,\"key4\":[10,\"value2\"],\"key5\":{\"key6\":17,\"key7\":37},\"key8\":null}'" in {
    Json.obj("key1" -> 12,
      "key2" -> "value2",
      "key3" -> true,
      "key4" -> Json.arr(10, "value2"),
      "key5" -> Json.obj("key6" -> 17,
        "key7" -> 37),
      "key8" -> JNull
    ).toJson shouldBe "{\"key4\":[10,\"value2\"],\"key5\":{\"key6\":17,\"key7\":37},\"key8\":null,\"key1\":12,\"key2\":\"value2\",\"key3\":true}"
  }

  behavior of "Checking of JSON deserialization"

  it should "deserialization of nested objects ({\"key\":{\"key2\":7}}) " in {
    JVal.parse("{\"key\":{\"key2\":7}}").toJson shouldBe "{\"key\":{\"key2\":7}}"
  }

  it should "deserialization of pure number (12) " in {
    JVal.parse("12").toJson shouldBe "12"
  }

  it should "deserialization of pure float number (12.7) " in {
    JVal.parse("12.7").toJson shouldBe "12.7"
  }

  it should "deserialization of pure string (\"text\") " in {
    JVal.parse("\"text\"").toJson shouldBe "\"text\""
  }

  it should "deserialization of pure boolean (false) " in {
    JVal.parse("false").toJson shouldBe "false"
  }

  it should "deserialization of pure boolean (true) " in {
    JVal.parse("true").toJson shouldBe "true"
  }

  it should "deserialization of an object with null field ({\"key\":null}) " in {
    JVal.parse("{\"key\":null}").toJson shouldBe "{\"key\":null}"
  }

  it should "deserialization of an complex object  ({\"key1\":null,\"key2\":7,\"key3\":12.7,\"key4\":\"some text\",\"key5\":true}) " in {
    JVal.parse("{\"key1\":null,\"key2\":7,\"key3\":12.7,\"key4\":\"some text\",\"key5\":true}").toJson shouldBe "{\"key4\":\"some text\",\"key5\":true,\"key1\":null,\"key2\":7,\"key3\":12.7}"
  }

  it should "deserialization of an pure array ([12,\"text\"]) " in {
    JVal.parse("[12, \"text\"]").toJson shouldBe "[12,\"text\"]"
  }

  it should "deserialization of an really complex object ({\"key1\":12,\"key2\":\"value2\",\"key3\":true,\"key4\":[10,\"value2\",{\"key\":7}],\"key5\":{\"key6\":17,\"key7\":37},\"key8\":null}) " in {
    JVal.parse("{\"key1\":12,\"key2\":\"value2\",\"key3\":true,\"key4\":[10,\"value2\", {\"key\":7}],\"key5\":{\"key6\":17,\"key7\":37},\"key8\":null}").toJson shouldBe "{\"key4\":[10,\"value2\",{\"key\":7}],\"key5\":{\"key6\":17,\"key7\":37},\"key8\":null,\"key1\":12,\"key2\":\"value2\",\"key3\":true}"
  }

  val obj = Json.obj(
    "name" -> "John",
    "lastname" -> "Smith",
    "address" -> Json.obj(
      "city" -> "Moscow"
    )
  )

  it should "extracting 'firstname' from " + obj.toJson + " " in {
    obj \ "firstname" shouldBe JUndef
  }

  it should "extracting 'lastname' from " + obj.toJson + " " in {
    obj \ "lastname" shouldBe JStr("Smith")
  }

  it should "extracting 'city' from " + obj.toJson + " " in {
    obj \ "address" \ "city" shouldBe JStr("Moscow")
  }

  it should "extracting 'address' from " + obj.toJson + " " in {
    obj \ "address"  shouldBe Json.obj("city" -> "Moscow")
  }

  behavior of "Checking of JSON marshalling"

  it should "marshalling Int " in {
    Json.toJson(7) shouldBe JNum(7)
  }

  it should "marshalling String " in {
    Json.toJson("text") shouldBe JStr("text")
  }

  it should "marshalling true " in {
    Json.toJson(true) shouldBe JBool(true)
  }

  it should "marshalling " + JArr(Array(JNum(1), JNum(2), JNum(3))) + " " in {
    Json.toJson(Array(1,2,3)).toString shouldBe JArr(Array(JNum(1), JNum(2), JNum(3))).toString
  }

  it should "marshalling " + Json.obj("key1" -> JNum(1), "key2" -> JNum(2), "key3" -> JNum(3)) + " " in {
    Json.toJson(Map("key1" -> 1, "key2" -> 2, "key3" -> 3)).toString shouldBe Json.obj("key1" -> JNum(1), "key2" -> JNum(2), "key3" -> JNum(3)).toString
  }

  it should "marshalling " + Seq(3, 5, 7) + " " in {
    Json.toJson(Seq(3, 5, 7)).toString shouldBe "JArr(JNum(3), JNum(5), JNum(7))"
  }

  it should "marshalling " + Option(7) + " " in {
    Json.toJson(Option(7)).toString shouldBe "JNum(7)"
  }

  it should "marshalling " + Option[String](null) + " " in {
    Json.toJson(Option[String](null)).toString shouldBe "JUndef"
  }

  behavior of "Checking of JSON unmarshalling"

  it should "unmarshalling Int " in {
    JNum(7).as[Int] shouldBe 7
  }

  it should "unmarshalling Int as Option" in {
    JNum(7).asOpt[Int] shouldBe Option(7)
  }

  it should "unmarshalling String " in {
    JStr("text").as[String] shouldBe "text"
  }

  it should "unmarshalling Boolean " in {
    JBool(true).as[Boolean] shouldBe true
  }

  it should "unmarshalling " + JArr(Array(JNum(1), JNum(2), JNum(3))) + " " in {
    JArr(Array(JNum(1), JNum(2), JNum(3))).as[Array[Int]].toSeq.toString shouldBe Array(1, 2, 3).toSeq.toString
  }

  it should "unmarshalling " + Json.obj("1" -> JNum(1), "2" -> JNum(2), "3" -> JNum(3)) + " " in {
    Json.obj("1" -> JNum(1), "2" -> JNum(2), "3" -> JNum(3)).as[Map[String,Int]].toMap.toString shouldBe Map("1" -> 1, "2" -> 2, "3" -> 3).toString
  }

  val objToTestOption = Json.obj("key1" -> "value1",
    "key2" -> "value2",
    "key3" -> "value3")

  it should "unmarshalling String as None " in {
    (objToTestOption \ "key4").asOpt[String] shouldBe None
  }

  it should "unmarshalling " + JArr(Array(JNum(3), JNum(5), JNum(7))) + " " in {
    JArr(Array(JNum(3), JNum(5), JNum(7))).as[Seq[Int]].toString shouldBe List(3,5,7).toString()
  }

  it should "unmarshalling " + JUndef + " to Option[Int] " in {
    JUndef.as[Option[Int]].toString shouldBe "None"
  }

  it should "unmarshalling " + JNum(7) + " to Option[Int] " in {
    JNum(7).as[Option[Int]].toString shouldBe "Some(7)"
  }

  behavior of "Checking of JSON constructing"

  it should "constructing JObj from name and value " in {
    JObj("key", JStr("value")).toJson shouldBe "{\"key\":\"value\"}"
  }

  it should "constructing JObj from a tuple (name, value) " in {
    JObj(("key", JStr("value"))).toJson shouldBe "{\"key\":\"value\"}"
  }

  val objToTestAdding = Json.obj(
    "name" -> "John",
    "lastname" -> "Smith"
  )

  behavior of "Checking of JSON adding"

  it should "adding an absent filed to JObj " in {
    (objToTestAdding + ("login", "jsmith")).toJson shouldBe "{\"name\":\"John\",\"lastname\":\"Smith\",\"login\":\"jsmith\"}"
  }

  it should "adding an existing filed to JObj " in {
    (objToTestAdding + ("name", "Mike")).toJson shouldBe "{\"name\":\"John\",\"lastname\":\"Smith\"}"
  }

  it should "adding or replace an absent filed to JObj " in {
    (objToTestAdding +! ("login", "jsmith")).toJson shouldBe "{\"name\":\"John\",\"lastname\":\"Smith\",\"login\":\"jsmith\"}"
  }

  val objToAdd = Json.obj(
    "name" -> "Mike",
    "login" -> "msmith"
  )

  it should "adding or replace one JObj to another JObj " in {
    (objToTestAdding ++! objToAdd).toJson shouldBe "{\"name\":\"Mike\",\"lastname\":\"Smith\",\"login\":\"msmith\"}"
  }

  val arr = Json.arr(10, "test", true)

  it should "prepend an element into an array " in {
    (7 +: arr).toJson shouldBe "[7,10,\"test\",true]"
  }

  it should "append an element into an array " in {
    (arr :+ 7).toJson shouldBe "[10,\"test\",true,7]"
  }

  val sub = Json.obj(
    "id" -> 193,
    "login" -> "jon",
    "trusted" -> true
  )

  import JsonTools._
  import scala.language.postfixOps

  case class SimpleSubject(id: Int) {
    override def toString: String = "SimpleSubject(id = " + id + ")"
  }

  object SimpleSubject {
    implicit def jreader = new JReader[SimpleSubject] {
      override def read(v: JVal): JResult[SimpleSubject] =
        ((v \ "id").read[Int] $).lift(SimpleSubject.apply)

    }
  }

  case class Subject(id: Int, login: String) {
    override def toString: String = "Subject(id = " + id + ", login = " + login + ")"
  }

  object Subject {
    implicit def jreader = new JReader[Subject] {
      override def read(v: JVal): JResult[Subject] =
        ((v \ "id").read[Int] and
          (v \ "login").read[String] $).lift(Subject.apply)

    }
  }

  case class ComplexSubject(id: Int, login: String, trusted: Boolean) {
    override def toString: String = "ComplexSubject(id = " + id + ", login = " + login + ", trusted = " + trusted + ")"
  }

  object ComplexSubject {
    implicit def jreader = new JReader[ComplexSubject] {
      override def read(v: JVal): JResult[ComplexSubject] =
        ((v \ "id").read[Int] and
          (v \ "login").read[String] and
          (v \ "trusted").read[Boolean] $).lift(ComplexSubject.apply)

    }
  }

  it should "deserialize simple JSON subject " in {
    sub.as[SimpleSubject] shouldBe SimpleSubject(193)
  }

  it should "deserialize normal JSON subject " in {
    sub.as[Subject] shouldBe Subject(193, "jon")
  }

  it should "deserialize complex JSON subject " in {
    sub.as[ComplexSubject] shouldBe ComplexSubject(193, "jon", true)
  }

  val usrWoLastName = Json.obj(
    "first_name" -> "jon"
  )

  val usr = Json.obj(
    "first_name" -> "jon",
    "last_name" -> "smith"
  )

  case class User(firstName: String, lastName: Option[String]) {
    override def toString: String = "User(firstName = " + firstName + ", lastName = " + lastName + ")"
  }

  object User {
    implicit def jreader = new JReader[User] {
      override def read(v: JVal): JResult[User] =
        ((v \ "first_name").read[String] and
          (v \ "last_name").read[Option[String]] $).lift(User.apply)

    }
  }

  it should "deserialize JSON user without last name " in {
    usrWoLastName.as[User] shouldBe User("jon", None)
  }

  it should "deserialize JSON user " in {
    usr.as[User] shouldBe User("jon", Some("smith"))
  }

}
