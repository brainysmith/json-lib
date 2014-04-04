#JSON-LIB
This project aims to simplify working with JSON objects

#Requires
* Please download latest version of SBT.
* [sbt 0.13.1](https://scala-sbt.org)
* [jackson-core-asl 1.9.10] (http://jackson.codehaus.org/)
* [jackson-mapper-asl 1.9.10] (http://jackson.codehaus.org/)

#Use
##Create JSON object:
```scala
Json.obj("key1" -> 12,
      "key2" -> "value2",
      "key3" -> true,
      "key4" -> Json.arr(10, "value2"),
      "key5" -> Json.obj("key6" -> 17,
      "key7" -> 37),
      "key8" -> JNull
    )
JObj("key", JStr("value"))
```
##Serialize to string:
```scala
Json.obj("key1" -> 12).toJson
```
##Deserialization from string:
```scala
JVal.parseStr("{\"key1\":null,\"key2\":7,\"key3\":12.7,\"key4\":\"some text\",\"key5\":true}")
```
##Extracting:
```scala
val obj = Json.obj(
    "name" -> "John",
    "lastname" -> "Smith",
    "address" -> Json.obj(
      "city" -> "Moscow"
    )
  )
obj \ "firstname" will be JUndef
obj \ "lastname" wll be JStr("Smith")
```
##Marshalling:
```scala
Json.toJson(Map("key1" -> 1, "key2" -> 2, "key3" -> 3)) will be Json.obj("key1" -> JNum(1), "key2" -> JNum(2), "key3" -> JNum(3))
Json.toJson(Array(1,2,3)) will be JArr(Array(JNum(1), JNum(2), JNum(3)))
```
##Unmarshalling:
```scala
JNum(7).asOpt[Int] will be Option(7)
JStr("text").as[String] will be "text"
JUndef.as[Option[Int]] will be None
```
##Adding adding:
```scala
val objToTestAdding = Json.obj(
  "name" -> "John",
  "lastname" -> "Smith"
)
//an absent filed
objToTestAdding + ("login", "jsmith") will be "{\"name\":\"John\",\"lastname\":\"Smith\",\"login\":\"jsmith\"}"
//adding an existing filed
objToTestAdding + ("name", "Mike") will be "{\"name\":\"John\",\"lastname\":\"Smith\"}"
//adding or replace an absent filed to JObj
objToTestAdding +! ("login", "jsmith") will be "{\"name\":\"John\",\"lastname\":\"Smith\",\"login\":\"jsmith\"}"
//adding or replace one JObj to another JObj
val objToAdd = Json.obj(
  "name" -> "Mike",
  "login" -> "msmith"
)
objToTestAdding ++! objToAdd will be "{\"name\":\"Mike\",\"lastname\":\"Smith\",\"login\":\"msmith\"}"
//prepend an element into an array
7 +: arr will be "[7,10,\"test\",true]"
```

##Author
