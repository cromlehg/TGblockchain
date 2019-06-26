package techgen

import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.ScorexEncoder
import scorex.util.serialization.{Reader, Writer}


case class Name(name: String) {

  override def equals(a: Any): Boolean = a match {
    case other: Name => name == other.name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode

  override lazy val toString: String = name

}

object NameSerializer extends ScorexSerializer[Name] {

  override def serialize(obj: Name, w: Writer): Unit = {
    val bytes = java.util.Base64.getEncoder.encode(obj.name.getBytes())
    w.putInt(bytes.size)
    w.putBytes(bytes)
  }

  override def parse(r: Reader): Name = {
    val size = r.getInt()
    val name = new String(java.util.Base64.getDecoder.decode(r.getBytes(size)))
    Name.tryCreate(name).right.get
  }

}

object Name extends ScorexEncoder {

  val maxSize = 12

  val minSize = 3

  val regexp = "[a-z]*"

  implicit val apiEncoder: Encoder[Name] =
    new Encoder[Name] {
      final def apply(a: Name) = a.name.asJson
    }

  def check(name: String): Either[String, String] =
    if (name.trim.length < 3)
      Left("Name must be greater than " + minSize + " symbols!")
    else if (name.trim.length >= maxSize)
      Left("Name must be less than " + (maxSize + 1) + " symbols!")
    else if (!name.trim.matches(regexp))
      Left("Name must checked with regexp " + regexp.toString)
    else
      Right(name.trim)

  def toMaxSize(name: String): String =
    name + (0 until (maxSize - name.trim.length)).map(_ => " ").mkString

  def create(name: String): Name =
    Name(toMaxSize(name))

  def tryCreate(name: String): Either[String, Name] =
    check(name).map(toMaxSize).map(Name(_))

}