package otechgen.old

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.util.ByteString
import play.api.libs.json._

import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NoStackTrace

case class PlayJsonException(cause: Option[Throwable] = None, errors: Seq[(JsPath, Seq[JsonValidationError])] = Seq.empty)
  extends IllegalArgumentException
    with NoStackTrace

trait APIMarshallers {

  private lazy val jsonStringMarshaller =
    Marshaller.stringMarshaller(`application/json`)

  implicit def playJsonMarshaller[A](implicit writes: Writes[A], printer: JsValue => String = Json.prettyPrint): ToEntityMarshaller[A] =
    jsonStringMarshaller.compose(printer).compose(writes.writes)

  private val jsonStringUnmarshaller =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .mapWithCharset {
        case (ByteString.empty, _) => throw Unmarshaller.NoContentException
        case (data, charset) => data.decodeString(charset.nioCharset.name)
      }

  implicit def playJsonUnmarshaller[A](implicit reads: Reads[A]): FromEntityUnmarshaller[A] =
    jsonStringUnmarshaller map { data =>
      val json = nonFatalCatch.withApply(t => throw PlayJsonException(cause = Some(t)))(Json.parse(data))
      json.validate[A] match {
        case JsSuccess(value, _) => value
        case JsError(errors) => throw PlayJsonException(errors = errors)
      }
    }

}

object APIMarshallers extends APIMarshallers