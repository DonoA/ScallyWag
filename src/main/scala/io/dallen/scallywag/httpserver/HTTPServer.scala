package io.dallen.scallywag.httpserver

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.Charset
import java.util.regex.Pattern

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class RequestBody()

case class RawBody(data: String) extends RequestBody
case class FormBody(data: Map[String, String]) extends RequestBody

case class HTTPRequest(
                        method: String,
                        location: String,
                        proto: String,
                        headers: Map[String, String],
                        urlParameters: Map[String, String],
                        var body: RequestBody)

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

object HTTPServer {
  private val requestPattern = Pattern.compile("^(?<method>\\w+)\\s" +
    "(?<path>[^\\s\\?]+)\\??(?<args>\\S*)\\s" +
    "HTTP\\/(?<httpVersion>[\\d\\.]+)\\r\\n" +
    "(?<headers>[\\w\\-]*\\:.+\\r\\n)+\\r\\n", Pattern.DOTALL)

  private val UTF8 = Charset.forName("UTF-8")
  private val ISO88591 = Charset.forName("ISO-8859-1")
}

class HTTPServer(port: Int, handler: HTTPRequest => HTTPResponse) {
  val tcpServer = new TCPServer(port, () => new StreamProcessor(consumeRequest))

  def start(): Unit = tcpServer.start()


  private def consumeRequest(request: HTTPRequest): (Array[Byte], Boolean) = {
    val rawResponse = handler.apply(request)
    rawResponse.headers.put("Content-Length", rawResponse.body.length.toString)
    return (serializeResponse(rawResponse).getBytes(HTTPServer.UTF8), true || shouldClose(rawResponse))
  }

  private def shouldClose(req: HTTPResponse, header: String = "Connection"): Boolean =
    req.headers.getOrElse(header, () => "close").equals("close")

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

  class StreamProcessor(consumeRequest: HTTPRequest => (Array[Byte], Boolean)) extends TCPConsumer {

    var bytesRead = 0

    var byteTarget: Option[Int] = None

    var workingRequest: Option[HTTPRequest] = None

    override def consume(buffer: ArrayBuffer[ByteBuffer], newRead: Int): (ByteBuffer, Boolean) = {
      var readBytes = newRead
      val bufferEnd = buffer.last.position()
      if(byteTarget.isEmpty) {
        val header = headerProcessor(buffer, readBytes)
        if(header.isEmpty) {
          return (ByteBuffer.wrap(Array[Byte]()), false)
        }
        workingRequest = Some(parseHeader(header.get))
        byteTarget = Some(workingRequest.get.headers.getOrElse("content-length", "0").toInt)

        val extraBytes = bufferEnd - buffer.last.position()
        val bytesToCopy = buffer.last
        buffer.clear()
        buffer.append(ByteBuffer.allocate(bytesToCopy.capacity()))
        while(bytesToCopy.hasRemaining) {
          buffer.last.put(bytesToCopy.get())
        }
        bytesRead = extraBytes
        readBytes = 0
      }

      if(byteTarget.isDefined) {
        val body = bodyProcessor(buffer, readBytes)
        if(body.isDefined) {
          workingRequest.get.body = RawBody(new String(body.get))
          val (data, close) = consumeRequest.apply(workingRequest.get)
          return (ByteBuffer.wrap(data), close)
        }
      }
      return (ByteBuffer.allocate(0), false)
    }

    private def headerProcessor(buffer: ArrayBuffer[ByteBuffer], newBytes: Int): Option[String] = {
      bytesRead += newBytes
      if (newBytes < 4 && buffer.size <= 1) {
        return None
      }

      val existingValues = buffer.last.position() - newBytes

      val prevBufferRes = if (buffer.size > 1 || existingValues > 4) {
        val prevBuff = buffer.apply(buffer.size - 2)
        prevBuff.position(prevBuff.capacity() - 4)
        checkBuffer(prevBuff, 0)
      } else {
        0
      }
      buffer.last.position(if(existingValues > 4) existingValues else 0)
      val latestBufferRes = checkBuffer(buffer.last, prevBufferRes)

      if(latestBufferRes >= 4) {
        val stringData = new String(joinBuffer(buffer, bytesRead), Charset.forName("UTF-8"))
        return Some(stringData)
      }
      return None
    }

    private def joinBuffer(arrayBuffer: ArrayBuffer[ByteBuffer], totalSize: Int): Array[Byte] =
      arrayBuffer.flatMap { e => e.array() }.toArray

    private def bodyProcessor(buffer: ArrayBuffer[ByteBuffer], newBytes: Int): Option[Array[Byte]] = {
      bytesRead += newBytes
      if(bytesRead >= byteTarget.get) {
        return Some(joinBuffer(buffer, bytesRead))
      } else {
        return None
      }
    }

    private def checkBuffer(byteBuffer: ByteBuffer, initPatternMatches: Int): Int = {
      var patternMatches = initPatternMatches
      while(byteBuffer.hasRemaining) {
        val current = byteBuffer.get()
        // check if this matches the pattern
        val expected = patternMatches match {
          case 0 | 2 => current == '\r'.toByte
          case 1 | 3 => current == '\n'.toByte
          case _ => return patternMatches
        }
        // if it does, advance state
        if(expected) {
          patternMatches += 1
        } else {
          patternMatches = 0
        }
        if(patternMatches == 4) {
          return patternMatches
        }
      }
      return patternMatches
    }

    private def parseHeader(stringMessage: String): HTTPRequest = {
      val regexMatch = HTTPServer.requestPattern.matcher(stringMessage)

      if (!regexMatch.find()) {
        throw new IllegalArgumentException("Data must represent an HTTP request")
      }

      val (method, path, httpVersion) =
        (regexMatch.group("method"), regexMatch.group("path"), regexMatch.group("httpVersion"))

      val urlParams = regexMatch
        .group("args")
        .split("&")
        .map { arg =>
          val data = arg.split("=", 2)
          if(data.size == 1) {
            (data(0), "true")
          } else {
            (data(0), data(1))
          }
        }.toMap

      val headers = regexMatch
        .group("headers")
        .split("\r\n")
        .map { header =>
          val Array(name, value) = header.split(": ", 2)
          (name, value)
        }.toMap

      return HTTPRequest(method, path, httpVersion, headers, urlParams, RawBody(""))
    }
  }
}
