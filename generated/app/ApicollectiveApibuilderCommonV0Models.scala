/**
 * Generated by API Builder - https://www.apibuilder.io
 * Service version: 0.13.29
 * apibuilder 0.14.3 app.apibuilder.io/apicollective/apibuilder-common/0.13.29/play_2_x_json
 */
package io.apibuilder.common.v0.models {

  case class Audit(
    createdAt: _root_.org.joda.time.DateTime,
    createdBy: io.apibuilder.common.v0.models.ReferenceGuid,
    updatedAt: _root_.org.joda.time.DateTime,
    updatedBy: io.apibuilder.common.v0.models.ReferenceGuid
  )

  case class Healthcheck(
    status: String
  )

  /**
   * Represents a reference to another model.
   */
  case class Reference(
    guid: _root_.java.util.UUID,
    key: String
  )

  case class ReferenceGuid(
    guid: _root_.java.util.UUID
  )

}

package io.apibuilder.common.v0.models {

  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._
    import io.apibuilder.common.v0.models.json._

    private[v0] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)

    private[v0] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
      def writes(x: java.util.UUID) = JsString(x.toString)
    }

    private[v0] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
      import org.joda.time.format.ISODateTimeFormat.dateTimeParser
      dateTimeParser.parseDateTime(str)
    }

    private[v0] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
      def writes(x: org.joda.time.DateTime) = {
        import org.joda.time.format.ISODateTimeFormat.dateTime
        val str = dateTime.print(x)
        JsString(str)
      }
    }

    private[v0] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
      import org.joda.time.format.ISODateTimeFormat.dateParser
      dateParser.parseLocalDate(str)
    }

    private[v0] implicit val jsonWritesJodaLocalDate = new Writes[org.joda.time.LocalDate] {
      def writes(x: org.joda.time.LocalDate) = {
        import org.joda.time.format.ISODateTimeFormat.date
        val str = date.print(x)
        JsString(str)
      }
    }

    implicit def jsonReadsApibuilderCommonAudit: play.api.libs.json.Reads[Audit] = {
      (
        (__ \ "created_at").read[_root_.org.joda.time.DateTime] and
        (__ \ "created_by").read[io.apibuilder.common.v0.models.ReferenceGuid] and
        (__ \ "updated_at").read[_root_.org.joda.time.DateTime] and
        (__ \ "updated_by").read[io.apibuilder.common.v0.models.ReferenceGuid]
      )(Audit.apply _)
    }

    def jsObjectAudit(obj: io.apibuilder.common.v0.models.Audit): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "created_at" -> play.api.libs.json.JsString(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(obj.createdAt)),
        "created_by" -> jsObjectReferenceGuid(obj.createdBy),
        "updated_at" -> play.api.libs.json.JsString(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(obj.updatedAt)),
        "updated_by" -> jsObjectReferenceGuid(obj.updatedBy)
      )
    }

    implicit def jsonWritesApibuilderCommonAudit: play.api.libs.json.Writes[Audit] = {
      new play.api.libs.json.Writes[io.apibuilder.common.v0.models.Audit] {
        def writes(obj: io.apibuilder.common.v0.models.Audit) = {
          jsObjectAudit(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderCommonHealthcheck: play.api.libs.json.Reads[Healthcheck] = {
      (__ \ "status").read[String].map { x => new Healthcheck(status = x) }
    }

    def jsObjectHealthcheck(obj: io.apibuilder.common.v0.models.Healthcheck): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "status" -> play.api.libs.json.JsString(obj.status)
      )
    }

    implicit def jsonWritesApibuilderCommonHealthcheck: play.api.libs.json.Writes[Healthcheck] = {
      new play.api.libs.json.Writes[io.apibuilder.common.v0.models.Healthcheck] {
        def writes(obj: io.apibuilder.common.v0.models.Healthcheck) = {
          jsObjectHealthcheck(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderCommonReference: play.api.libs.json.Reads[Reference] = {
      (
        (__ \ "guid").read[_root_.java.util.UUID] and
        (__ \ "key").read[String]
      )(Reference.apply _)
    }

    def jsObjectReference(obj: io.apibuilder.common.v0.models.Reference): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "guid" -> play.api.libs.json.JsString(obj.guid.toString),
        "key" -> play.api.libs.json.JsString(obj.key)
      )
    }

    implicit def jsonWritesApibuilderCommonReference: play.api.libs.json.Writes[Reference] = {
      new play.api.libs.json.Writes[io.apibuilder.common.v0.models.Reference] {
        def writes(obj: io.apibuilder.common.v0.models.Reference) = {
          jsObjectReference(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderCommonReferenceGuid: play.api.libs.json.Reads[ReferenceGuid] = {
      (__ \ "guid").read[_root_.java.util.UUID].map { x => new ReferenceGuid(guid = x) }
    }

    def jsObjectReferenceGuid(obj: io.apibuilder.common.v0.models.ReferenceGuid): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "guid" -> play.api.libs.json.JsString(obj.guid.toString)
      )
    }

    implicit def jsonWritesApibuilderCommonReferenceGuid: play.api.libs.json.Writes[ReferenceGuid] = {
      new play.api.libs.json.Writes[io.apibuilder.common.v0.models.ReferenceGuid] {
        def writes(obj: io.apibuilder.common.v0.models.ReferenceGuid) = {
          jsObjectReferenceGuid(obj)
        }
      }
    }
  }
}

package io.apibuilder.common.v0 {

  object Bindables {

    import play.api.mvc.{PathBindable, QueryStringBindable}
    import org.joda.time.{DateTime, LocalDate}
    import org.joda.time.format.ISODateTimeFormat
    import io.apibuilder.common.v0.models._

    // Type: date-time-iso8601
    implicit val pathBindableTypeDateTimeIso8601 = new PathBindable.Parsing[org.joda.time.DateTime](
      ISODateTimeFormat.dateTimeParser.parseDateTime(_), _.toString, (key: String, e: _root_.java.lang.Exception) => s"Error parsing date time $key. Example: 2014-04-29T11:56:52Z"
    )

    implicit val queryStringBindableTypeDateTimeIso8601 = new QueryStringBindable.Parsing[org.joda.time.DateTime](
      ISODateTimeFormat.dateTimeParser.parseDateTime(_), _.toString, (key: String, e: _root_.java.lang.Exception) => s"Error parsing date time $key. Example: 2014-04-29T11:56:52Z"
    )

    // Type: date-iso8601
    implicit val pathBindableTypeDateIso8601 = new PathBindable.Parsing[org.joda.time.LocalDate](
      ISODateTimeFormat.yearMonthDay.parseLocalDate(_), _.toString, (key: String, e: _root_.java.lang.Exception) => s"Error parsing date $key. Example: 2014-04-29"
    )

    implicit val queryStringBindableTypeDateIso8601 = new QueryStringBindable.Parsing[org.joda.time.LocalDate](
      ISODateTimeFormat.yearMonthDay.parseLocalDate(_), _.toString, (key: String, e: _root_.java.lang.Exception) => s"Error parsing date $key. Example: 2014-04-29"
    )



  }

}
