package io.dallen.scallywag.httpserver

import java.nio.channels.SocketChannel

case class HTTPRequest(
                        method: String,
                        location: String,
                        proto: String,
                        headers: Map[String, String],
                        urlParameters: Map[String, String],
                        body: String)

case class HTTPResponse(
                         var code: HTTPResponseCode,
                         var headers: Map[String, String],
                         var body: String)

case class HTTPResponseCode(code: Int, msg: String) {
  override def toString: String = code.toString + " " + msg
}

object HTTPResponseCode {
  val OK = HTTPResponseCode(200, "OK")
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

  private def tcpHandler(message: String, channel: SocketChannel): String = {
    val req = parseHTTPRequest(message)
    val rawResponse = handler.apply(req)
    return serializeResponse(rawResponse)
  }

  private def parseHTTPRequest(message: String): HTTPRequest = {
    val lines = message.split("\r\n")
    val firstLine = lines(0).split(" ")
    val (method, location, proto) = (firstLine(0), firstLine(1), firstLine(2))
    val headers = lines.drop(1).map(s => s.splitAt(s.indexOf(":"))).toMap
    return HTTPRequest(method, location, proto, headers, Map[String, String](), "")
  }

  private def serializeResponse(httpResponse: HTTPResponse): String = {
    val responseString = new StringBuilder()
    responseString.append("HTTP/1.1 ")
      .append(httpResponse.code.toString)
      .append("\r\n")
    httpResponse.headers.foreach(header => responseString.append(header._1).append(": ").append(header._2).append("\r\n"))
    responseString.append("\r\n\r\n").append(httpResponse.body)
    return responseString.toString()
  }

}
