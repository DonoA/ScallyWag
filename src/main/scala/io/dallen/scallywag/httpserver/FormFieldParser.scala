package io.dallen.scallywag.httpserver

import java.nio.charset.Charset
import java.util.regex.Pattern

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object FormFieldParser {
  case class FormField(contentDisposition: ContentDisposition, contentType: Option[ContentType], data: Array[Byte]) {
    def getCharset: Charset = contentType match {
      case Some(ct) => ct.charset match {
        case Some(cs) => Charset.forName(cs)
        case None => HTTPServer.UTF8
      }
      case None => HTTPServer.UTF8
    }
  }
  case class ContentDisposition(name: String, filename: Option[String])
  case class ContentType(name: String, charset: Option[String])

  private val headerPattern = Pattern.compile("Content-Disposition: (?<type>.+);\\sname=\"(?<name>.+)\"")
}

class FormFieldParser(data: Array[Byte], boarder: Array[Byte]) {

  private val fields = new ArrayBuffer[FormFieldParser.FormField]()

  private var currentPos = 0

  private val headerBoarder = "\r\n\r\n".getBytes(HTTPServer.UTF8)

  private val dataTerminator = "\r\n".getBytes(HTTPServer.UTF8)

  def parse(): Unit = {
    consumeTo(boarder ++ dataTerminator)
    while(true) {
      val header = new String(consumeTo(headerBoarder), HTTPServer.UTF8)
      if(header.isEmpty) {
        return
      }
      val data = consumeTo(dataTerminator ++ boarder ++ dataTerminator)

      fields.append(buildFormField(header, data))
    }
  }

  def consumeTo(pattern: Array[Byte]): Array[Byte] = {
    val startPos = currentPos
    var patternStart = 0
    var currentMatching = 0
    while(currentPos < data.length) {
      if(data(currentPos) == pattern(currentMatching)) {
        if(currentMatching == 0) {
          patternStart = currentPos
        }
        currentMatching += 1
      }

      if(currentMatching >= pattern.length) {
        return data.slice(startPos + 1, patternStart)
      }
      currentPos += 1
    }
    return Array[Byte]()
  }

  private def buildFormField(header: String, data: Array[Byte]): FormFieldParser.FormField = {
    val parts = header
      .split("\n")
      .map(_.split(": ", 2))
      .map {
        case Array(name, content) => (name.toLowerCase, content)
        case _ => throw new HTTPServer.MalformedRequestException(s"Form header $header did not match pattern 'header-name: header-data'")
      }.map { case (name, content) =>
        val subHeader = content
          .split("; ")
          .map(_.split("="))
          .map {
            case Array(seg_name, value) => (seg_name.toLowerCase, stripQuotes(value))
            case Array(seg_name) => ("base", seg_name.toLowerCase)
            case _ => throw new HTTPServer.MalformedRequestException(s"Form header $header does not have semicolon separated values")
          }.toMap
        (name, subHeader)
      }.toMap

    val noHeader = (name: String) => new HTTPServer.MalformedRequestException(s"Form header $header does not have $name")

    val formDisposition = FormFieldParser.ContentDisposition(
      parts
        .getOrElse("content-disposition", throw noHeader("Content-Disposition"))
        .getOrElse("name", throw noHeader("name")),
      parts
        .getOrElse("content-disposition", throw noHeader("Content-Disposition"))
        .get("filename")
    )

    val formType = parts.get("content-type").map {
      subHeaders => FormFieldParser.ContentType(
        subHeaders
          .getOrElse("base", throw noHeader("content-type")),
        subHeaders
          .get("chatset")
        )
    }
    FormFieldParser.FormField(formDisposition, formType, data)
  }

  def asFormData(): Map[String, ArrayBuffer[RequestBody.FormData]] = {
    val fieldMap = new mutable.HashMap[String, ArrayBuffer[RequestBody.FormData]]()
    fields.foreach { field =>
      val dataList = fieldMap.getOrElseUpdate(field.contentDisposition.name, new ArrayBuffer[RequestBody.FormData]())
      if(field.contentDisposition.filename.isDefined) {
        dataList.append(RequestBody.FormFile(
          field.contentType.getOrElse(FormFieldParser.ContentType("text/plain", None)).name,
          field.contentDisposition.filename.get,
          field.data))
      } else {
        dataList.append(RequestBody.FormString(new String(field.data, field.getCharset)))
      }
    }
    return fieldMap.toMap
  }

  private def stripQuotes(string: String): String = if(string.startsWith("\"") && string.endsWith("\"")) {
    string.slice(1, string.length - 1)
  } else {
    string
  }

  def getFields: ArrayBuffer[FormFieldParser.FormField] = fields
}
