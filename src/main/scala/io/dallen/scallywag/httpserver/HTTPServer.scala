package io.dallen.scallywag.httpserver

import java.nio.channels.SocketChannel

import scala.collection.mutable

case class HTTPRequest(
                        method: String,
                        location: String,
                        proto: String,
                        headers: Map[String, String],
                        urlParameters: Map[String, String],
                        body: String)

case class HTTPResponse(
                         var code: HTTPResponseCode,
                         var headers: mutable.Map[String, String],
                         var body: String)

case class HTTPResponseCode(code: Int, msg: String) {
  override def toString: String = code.toString + " " + msg
}

object HTTPResponseCode {
  val OK = HTTPResponseCode(200, "OK")
  val NOT_FOUND = HTTPResponseCode(404, "NOT FOUND")
  val ERROR = HTTPResponseCode(500, "SERVER ERROR")

}

case class HTTPMethod(name: String)

object HTTPMethod {
  val GET = HTTPMethod("GET")
  val POST = HTTPMethod("POST")

  def getByName(name: String): HTTPMethod = name match {
    case "GET" => HTTPMethod.GET
    case "POST" => HTTPMethod.POST
  }
}

class HTTPServer(port: Int, handler: HTTPRequest => HTTPResponse) {
  val tcpServer = new TCPServer(port, tcpHandler)

  def start(): Unit = tcpServer.start()

  private def tcpHandler(message: String, channel: SocketChannel): (String, Boolean) = {
    val req = parseHTTPRequest(message)
    val rawResponse = handler.apply(req)
    rawResponse.headers.put("Content-Length", rawResponse.body.length.toString)
    return (serializeResponse(rawResponse), true || shouldClose(rawResponse))
  }

  private def shouldClose(req: HTTPResponse, header: String = "Connection"): Boolean =
    req.headers.getOrElse(header, () => "close").equals("close")

  private def parseHTTPRequest(message: String): HTTPRequest = {
    val lines = message.split("\r\n")
    val firstLine = lines(0).split(" ")
    val (method, url, proto) = (firstLine(0), firstLine(1), firstLine(2))
    var location = url
    var urlParams = Map[String, String]()
    val paramSplitPoint = url.indexOf("?")
    if(paramSplitPoint != -1) {
      val urlBits = url.splitAt(paramSplitPoint)
      location = urlBits._1
      urlParams = urlBits._2.drop(1).split("&").map(p => {
        val b = p.splitAt(p.indexOf("="))
        (b._1, b._2.drop(1))
      }).toMap
    }
    val bodyStart = lines.indexOf("")
    var headerLines = lines.drop(1)
    var body = ""
    if(bodyStart != -1) {
      val (newHeaderLines, newBodyLines) = lines.drop(1).splitAt(bodyStart)
      body = newBodyLines.fold("")(_.concat(_).concat("\r\n"))
      headerLines = newHeaderLines
    }
    val headers = headerLines.dropRight(1).map(s => {
      val b = s.splitAt(s.indexOf(":"))
      (b._1, b._2.drop(2))
    }).toMap
    return HTTPRequest(method, location, proto, headers, urlParams, body)
  }

  private def serializeResponse(httpResponse: HTTPResponse): String = {
    val code = httpResponse.code.toString
    val headers = httpResponse.headers.map { case (name, value) => s"$name: $value" }.mkString("\r\n")
    val body = httpResponse.body
    s""" |HTTP/1.1 $code
         |$headers
         |
         |$body
         |
       """.stripMargin.replace("\n", "\r\n")
  }

}
