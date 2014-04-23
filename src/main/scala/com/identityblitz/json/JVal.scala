package com.identityblitz.json

import org.codehaus.jackson.map._
import org.codehaus.jackson.`type`.JavaType
import org.codehaus.jackson.{JsonToken, JsonParser, Version, JsonGenerator}
import org.codehaus.jackson.map.annotate.JsonCachable
import org.codehaus.jackson.map.module.SimpleModule
import org.codehaus.jackson.map.Module.SetupContext
import java.io.StringWriter
import org.codehaus.jackson.map.`type`.TypeFactory
import scala.collection.mutable
import scala.collection.generic.CanBuildFrom


/**
 *
 */
trait JVal {

  /**
   * Extract a value corresponding to filed name passed, from this object. It works only in JSON objects.
   * @param filed - field name
   * @return - value - if found;
   *           JUndef - if not found.
   */
  def \(filed: String): JVal = JUndef

  /**
   * Serializes this object to string
   * @return a string in JSON format
   */
  def toJson: String = JacksonBridge.jVal2JsonString(this)

  /**
   * Unmarshal a JVal object into an object of type T.
   * @param reader - reader being used to unmarshal
   * @tparam T - type to unmarshal in
   * @return - an unmarshalled object
   */
  def as[T](implicit reader: JReader[T]) = reader.read(this).fold[T](e => throw new IllegalStateException(e.toString()))(v => v)

  /**
   * Unmarshal a JVal object into an Option containing object of type T.
   * @param reader - reader being used to unmarshal
   * @tparam T - type to unmarshal in
   * @return - an Option of unmarshalled object or None if any errors occurred
   */
  def asOpt[T](implicit reader: JReader[T]): Option[T] = reader.read(this).fold[Option[T]](_ => None)(v => Option(v))

}

object JVal {

  /**
   * Parses input string containing a JSON object.
   * @param str - string containing a JSON object.
   * @return - a JSON object
   */
  def parseStr(str: String): JVal = JacksonBridge.jsonString2JVal(str)
}

case object JUndef extends JVal

case object JNull extends JVal

case class JNum(private val value: BigDecimal) extends JVal

object JNum {
  implicit def bigDecimalConverter(j: JNum): BigDecimal = j.value
}

case class JStr(private val value: String) extends JVal

object JStr {
  implicit def stringConverter(j: JStr): String = j.value
}

case class JBool(private val value: Boolean) extends JVal

object JBool {
  implicit def booleanConverter(j: JBool): Boolean = j.value
}

case class JArr(private val v: Array[JVal]) extends JVal {

  def this(b: mutable.Buffer[JVal]) = this(b.toArray)

  private val value: Array[JVal] = v.clone()

  def apply[B <: JVal](idx: Int): B = value(idx).asInstanceOf[B]

  def +:[T](element: T)(implicit writer: JWriter[T]) = JArr.prepend(this, element)
  def :+[T](element: T)(implicit writer: JWriter[T]) = JArr.append(this, element)

  def foreach(f: (JVal) => Unit): Unit = value.foreach(f)

  def map[B](f: (JVal) => B)(implicit bf: CanBuildFrom[Array[JVal], B, Array[B]]): Array[B] = value.map(f)(bf)

  def forall(f: (JVal) => Boolean): Boolean = value.forall(f)

  override def toString = v.mkString("JArr(", ", ", ")")

}

object JArr {

  def prepend[T](arr: JArr, element: T)(implicit writer: JWriter[T]): JArr = JArr((writer.write(element) +: arr.value.toSeq).toArray)

  def append[T](arr: JArr, element: T)(implicit writer: JWriter[T]): JArr = JArr((arr.value.toSeq :+ writer.write(element)).toArray)

}

case class JObj(private val v: Seq[(String, JVal)]) extends JVal {

  private[json] val value = v.toMap

  def apply(name: String):JVal = value.get(name).getOrElse(JUndef)

  def +[T](name: String, value: T)(implicit writer: JWriter[T]): JObj = JObj.add(this, name, value)
  def +[T](v: (String, T))(implicit writer: JWriter[T]): JObj = this + (v._1, v._2)

  def +![T](name: String, value: T)(implicit writer: JWriter[T]): JObj = JObj.addOrReplace(this, name, value)
  def +![T](v: (String, T))(implicit writer: JWriter[T]): JObj = this +! (v._1, v._2)

  def ++!(that: JObj): JObj = JObj.addOrReplace(this, that)

  def fields: Set[String] = value.keySet

  def isEmpty: Boolean = value.isEmpty

  def foreach(f: ((String, JVal)) => Unit): Unit = value.foreach(f)

  def map[B, That](f: ((String, JVal)) => B)(implicit bf: CanBuildFrom[Map[String, JVal], B, That]): That = value.map(f)(bf)

  def forall(f: ((String, JVal)) => Boolean): Boolean = value.forall(f)

  override def \(field: String) = value.get(field).getOrElse(JUndef)

}

object JObj {

  def apply(): JObj = new JObj(Seq())
  def apply(name: String, value: JVal): JObj = new JObj(Seq((name, value)))
  def apply(v: (String, JVal)): JObj = new JObj(Seq(v))

  @inline
  private def add[T](obj: JObj, fieldName: String, fieldValue: T)(implicit writer: JWriter[T]): JObj = if (obj.value.contains(fieldName)) obj else JObj(obj.value.toSeq :+ ((fieldName, Json.toJson(fieldValue))))

  @inline
  private def addOrReplace[T](obj: JObj, fieldName: String, fieldValue: T)(implicit writer: JWriter[T]): JObj = JObj(obj.value.toSeq :+ ((fieldName, Json.toJson(fieldValue))))

  @inline
  private def addOrReplace(obj: JObj, that: JObj): JObj = JObj(obj.value.toSeq ++ that.value.toSeq)

}

@JsonCachable
private[json] class JSerializer extends JsonSerializer[JVal]{
  def serialize(bean: JVal, generator: JsonGenerator, provider: SerializerProvider) {
    bean match {
      case JNull => generator.writeNull()
      case JNum(v) => generator.writeNumber(v.bigDecimal)
      case JStr(v) => generator.writeString(v)
      case JBool(v) => generator.writeBoolean(v)
      case JArr(v) => {
        generator.writeStartArray()
        v.foreach(serialize(_, generator, provider))
        generator.writeEndArray()
      }
      case o: JObj => {
        generator.writeStartObject()
        o.value.foreach(e => {
          e._2 match {
            case JUndef =>
            case v => {
              generator.writeFieldName(e._1)
              serialize(v, generator, provider)
            }
          }
        })
        generator.writeEndObject()
      }
    }
  }
}

object JElemType extends Enumeration {
  type JElemType = Value
  val SIMPLE, OBJECT, ARRAY, FIELD = Value
}
import JElemType._

trait JVisitor {
  def visit(l: JElemType, s: AnyRef)
  def visit(l: JElemType)(f: AnyRef=>JVal)
  def visit(e: JVal)
  def produce: JVal
}

class JValVisitor extends JVisitor {

  var level = SIMPLE
  val levels = mutable.Stack[(JElemType, AnyRef)]()
  var cur: AnyRef = null
  var finalElem: JVal = _

  @inline
  private def popLevel = {
    val old = cur
    levels.pop() match {
      case (l, c) => level = l; cur = c
    }
    old
  }

  @inline
  private def fieldEnd(v: JVal) {
    val fieldName = popLevel.asInstanceOf[String]
    if(level != OBJECT)
      throw new IllegalStateException("Mailformed JSON. After finishing a filed found level = " + level + " on the stack")
    cur.asInstanceOf[mutable.ListBuffer[(String, JVal)]] += ((fieldName, v))
  }

  def visit(l: JElemType, s: AnyRef) {
    if(level != SIMPLE) {
      levels.push((level, cur))
    }
    cur = s
    level = l
  }

  def visit(l: JElemType)(f: AnyRef=>JVal) {
    if(level != l) {
      throw new IllegalStateException("Malformed JSON. Called for level '" + l + "' but current level is " + level)
    }
    val value = f(cur)
    if(levels.isEmpty) {
      finalElem = value
    }
    else {
      popLevel
      level match {
        case ARRAY => cur.asInstanceOf[mutable.ListBuffer[JVal]] += value
        case FIELD => fieldEnd(value)
        case _ @ lvl => throw new IllegalStateException("Malformed JSON. After ending level '" + lvl + "' on stack found level = " + lvl)
      }
    }
  }

  def visit(e: JVal) {
    if(level == SIMPLE) {
      finalElem = e
    }
    else {
      level match {
        case ARRAY => cur.asInstanceOf[mutable.ListBuffer[JVal]] += e
        case FIELD => fieldEnd(e)
        case _ @ lvl => throw new IllegalStateException("Malformed JSON. After JNum on stack found level = " + lvl)
      }
    }
  }

  def produce: JVal = finalElem
}

@JsonCachable
private[json] class JDeserializer(factory: TypeFactory, klass: Class[_]) extends JsonDeserializer[Object] {
  def deserialize(jp: JsonParser, ctx: DeserializationContext): JVal = {

    val visitor: JVisitor = new JValVisitor

    if(!jp.hasCurrentToken)
      jp.nextToken()
    var tkn = jp.getCurrentToken

    while(tkn != null) {
      tkn match {
        case JsonToken.NOT_AVAILABLE => throw new IllegalStateException("A JSON stream has finished unexpectedly")
        case JsonToken.VALUE_EMBEDDED_OBJECT => throw new IllegalStateException("An embedded object found.")
        case _ => accept(tkn, jp, visitor)
      }
      tkn = jp.nextToken()
    }

    val value = visitor.produce
    if (!klass.isAssignableFrom(value.getClass)) {
      throw ctx.mappingException(klass)
    }
    value
  }

  @inline
  def accept(tkn: JsonToken, jp: JsonParser, visitor: JVisitor) {
    tkn match {
      case JsonToken.START_OBJECT => visitor.visit(OBJECT, new mutable.ListBuffer[(String, JVal)]())
      case JsonToken.FIELD_NAME => visitor.visit(FIELD, jp.getCurrentName)
      case JsonToken.END_OBJECT => visitor.visit(OBJECT)(s => new JObj(s.asInstanceOf[mutable.ListBuffer[(String, JVal)]]))
      case JsonToken.START_ARRAY => visitor.visit(ARRAY, new mutable.ListBuffer[JVal]())
      case JsonToken.END_ARRAY => visitor.visit(ARRAY)(s => new JArr(s.asInstanceOf[mutable.ListBuffer[JVal]]))
      case JsonToken.VALUE_NUMBER_INT => visitor.visit(JNum(jp.getDecimalValue))
      case JsonToken.VALUE_NUMBER_FLOAT => visitor.visit(JNum(jp.getDecimalValue))
      case JsonToken.VALUE_NULL => visitor.visit(JNull)
      case JsonToken.VALUE_FALSE => visitor.visit(JBool(jp.getBooleanValue))
      case JsonToken.VALUE_TRUE => visitor.visit(JBool(jp.getBooleanValue))
      case JsonToken.VALUE_STRING => visitor.visit(JStr(jp.getText))
      case _ => throw new IllegalStateException("Found unknown token " + tkn)
    }
  }

}

private[json] class JSerializers extends Serializers.Base {
  override def findSerializer(config: SerializationConfig, javaType: JavaType, beanDesc: BeanDescription, property: BeanProperty) =
    if(classOf[JVal].isAssignableFrom(beanDesc.getBeanClass)) {
      new JSerializer
    }
    else {
      null
    }.asInstanceOf[JsonSerializer[Object]]
}

private[json] class JDeserializers extends Deserializers.Base {
  override def findBeanDeserializer(javaType: JavaType, config: DeserializationConfig,
                                    provider: DeserializerProvider, beanDesc: BeanDescription,
                                    property: BeanProperty) = {
    val klass = javaType.getRawClass
    if (classOf[JVal].isAssignableFrom(klass)) {
      new JDeserializer(config.getTypeFactory, klass)
    } else null
  }

}

private[json] object JacksonBridge {

  val mapper = new ObjectMapper

  object module extends SimpleModule("BlitzModule", new Version(1, 0, 0, null)) {
    override def setupModule(context: SetupContext) {
      context.addSerializers(new JSerializers)
      context.addDeserializers(new JDeserializers)
    }
  }
  mapper.registerModule(module)

  private[this] val factory = mapper.getJsonFactory

  private[this] def generator(output: StringWriter) = factory.createJsonGenerator(output)

  private[this] def jsonParser(s: String) = factory.createJsonParser(s)

  private[json] def jVal2JsonString(value: JVal): String = {
    val writer = new StringWriter
    mapper.writeValue(generator(writer), value)
    writer.flush()
    writer.toString
  }

  private[json] def jsonString2JVal(strJson: String): JVal = {
    mapper.readValue(jsonParser(strJson), classOf[JVal])
  }

}




