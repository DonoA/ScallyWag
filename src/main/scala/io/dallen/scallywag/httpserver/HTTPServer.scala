package io.dallen.scallywag.httpserver

import java.nio.charset.Charset
import java.util.regex.Pattern

import io.dallen.scallywag.httpserver.HTTPServer.{Request, Response}
import io.dallen.scallywag.tcpserver.TCPServer

import scala.collection.mutable

object HTTPServer {
  val requestPattern: Pattern = Pattern.compile("^(?<method>\\w+)\\s" +
    "(?<path>[^\\s\\?]+)\\??(?<args>\\S*)\\s" +
    "HTTP\\/(?<httpVersion>[\\d\\.]+)\\r\\n" +
    "(?<headers>[\\w\\-]*\\:.+\\r\\n)*\\r\\n", Pattern.DOTALL)

  val UTF8: Charset = Charset.forName("UTF-8")
  val ISO88591: Charset = Charset.forName("ISO-8859-1")

  case class Request(
                      method: String,
                      location: String,
                      proto: String,
                      headers: Map[String, String],
                      urlParameters: Map[String, String],
                      var body: RequestBody)

  case class Response(
                       var code: ResponseCode,
                       var headers: mutable.Map[String, String],
                       var body: String)

  case class Method(name: String)

  object Method {
    val GET = Method("GET")
    val POST = Method("POST")

    def getByName(name: String): Method = name match {
      case "GET" => Method.GET
      case "POST" => Method.POST
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
}

class HTTPServer(port: Int, handler: Request => Response, var tcpServer: TCPServer) {

  def start(): Unit = tcpServer.start()

  def await(): Unit = tcpServer.await()

  def stop(): Unit = tcpServer.stop()

  def this(port: Int, handler: Request => Response) {
    this(port, handler, null)
    tcpServer = new TCPServer(port, () => new TCPStreamCollector(consumeRequest).consume _)
  }

  private def consumeRequest(request: Request): (Array[Byte], Boolean) = {
    val rawResponse = handler.apply(request)
    rawResponse.headers.put("content-length", rawResponse.body.getBytes.length.toString)
    val keepAlive = !shouldClose(rawResponse)
    if(keepAlive) {
      println("Keeping socket open")
    }
    return (serializeResponse(rawResponse), !keepAlive)
  }

  private def shouldClose(req: Response, header: String = "Connection"): Boolean =
    req.headers.getOrElse(header, () => "close").equals("close")

  private def serializeResponse(httpResponse: Response): Array[Byte] = {
    val code = httpResponse.code.toString
    val headers = httpResponse.headers.map { case (name, value) => s"$name: $value" }.mkString("\r\n")
    val body = httpResponse.body
    return s""" |HTTP/1.1 $code
                |$headers
                |
                |$body
                |
              """
      .stripMargin
      .replace("\n", "\r\n")
      .getBytes(HTTPServer.UTF8)
  }
}
