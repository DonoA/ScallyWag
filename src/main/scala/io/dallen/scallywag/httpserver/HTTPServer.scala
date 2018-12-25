package io.dallen.scallywag.httpserver

import java.nio.charset.Charset
import java.util.regex.Pattern

import io.dallen.scallywag.httpserver.HTTPServer.{Request, Response}
import io.dallen.scallywag.tcpserver.TCPServer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

object HTTPServer {
  val requestPattern: Pattern = Pattern.compile("^(?<method>\\w+)\\s" +
    "(?<path>[^\\s\\?]+)\\??(?<args>\\S*)\\s" +
    "HTTP\\/(?<httpVersion>[\\d\\.]+)\\r\\n" +
    "(?<headers>[\\w\\-]*\\:.+\\r\\n)*\\r\\n", Pattern.DOTALL)

  val UTF8: Charset = Charset.forName("UTF-8")
  val ISO88591: Charset = Charset.forName("ISO-8859-1")

  case class Request(method: HTTPServer.Method,
                      location: String,
                      proto: String,
                      headers: Map[String, String],
                      urlParameters: Map[String, String],
                      var body: RequestBody)

  case class Response(var code: ResponseCode,
                       var headers: mutable.Map[String, String],
                       var body: String)

  case class Method(name: String)

  object Method {
    val GET = Method("GET")
    val POST = Method("POST")
    val ANY = Method("ANY")

    def getByName(name: String): Method = name match {
      case "GET" => Method.GET
      case "POST" => Method.POST
      case _ => Method.ANY
    }
  }

  case class ResponseCode(code: Int, msg: String) {
    override def toString: String = code.toString + " " + msg
  }

  object ResponseCode {
    val OK = ResponseCode(200, "OK")
    val NOT_FOUND = ResponseCode(404, "NOT FOUND")
    val ERROR = ResponseCode(500, "SERVER ERROR")
  }

  class MalformedRequestException(msg: String) extends Exception
}

class HTTPServer(port: Int, handler: Request => Response, var tcpServer: TCPServer) {

  def start(): Future[Unit] = tcpServer.start()

  def stop(): Unit = tcpServer.stop()

  def this(port: Int, handler: Request => Response) {
    this(port, handler, null)
    tcpServer = new TCPServer(port, () => new TCPStreamCollector(consumeRequest).consume _)
  }

  private def parseBody(body: Array[Byte], rawReqType: String): RequestBody = rawReqType match {
    case "plain/text" => RequestBody.StringBody(new String(body, HTTPServer.UTF8))
    case "application/x-www-form-urlencoded" =>
      RequestBody.FormBody(new String(body, HTTPServer.UTF8)
        .split("&")
        .map(field => field.split("="))
        .map { case Array(data_name, data_value) => (data_name, ArrayBuffer[RequestBody.FormData](RequestBody.FormString(data_value))) }
        .toMap)
    case a if a.contains("multipart/form-data") => {
      val boundBytes = a.split("; ")(1).split("=")(1).getBytes(HTTPServer.UTF8)
      val fieldParser = new FormFieldParser(body, boundBytes)
      fieldParser.parse()
      RequestBody.FormBody(fieldParser.asFormData())
    }
    case _ => RequestBody.RawBody(body)
  }

  private def consumeRequest(rawReq: TCPStreamCollector.RawRequest): TCPStreamCollector.RawResponse = {
    val body: RequestBody = parseBody(rawReq.body, rawReq.headers.getOrElse("content-type", "plain/text"))

    val request = HTTPServer.Request(
      HTTPServer.Method.getByName(rawReq.method),
      rawReq.location,
      rawReq.proto,
      rawReq.headers,
      rawReq.urlParameters,
      body
    )
    val rawResponse = handler.apply(request)
    rawResponse.headers.put("content-length", rawResponse.body.getBytes.length.toString)
    val keepAlive = !shouldClose(rawResponse)
    val bytesResponse = serializeResponse(rawResponse)
    println(new String(bytesResponse))
    return TCPStreamCollector.RawResponse(bytesResponse, !keepAlive)
  }

  private def shouldClose(req: Response, header: String = "connection"): Boolean =
    req.headers.getOrElse(header, () => "close").equals("close")

  private def serializeResponse(httpResponse: Response): Array[Byte] = {
    val code = httpResponse.code.toString
    val headers = httpResponse.headers.map { case (name, value) => s"$name: $value" }.mkString("\n")
    val body = httpResponse.body
    return s""" |HTTP/1.1 $code
                |$headers
                |
                |$body"""
      .stripMargin
      .replace("\n", "\r\n")
      .getBytes(HTTPServer.UTF8)
  }
}
