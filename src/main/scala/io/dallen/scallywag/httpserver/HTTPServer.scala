package io.dallen.scallywag.httpserver

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util.regex.Pattern

import io.dallen.scallywag.tcpserver.TCPServer

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
  val tcpServer = new TCPServer(port, () => new StreamProcessor(consumeRequest).consume _)

  def start(): Unit = tcpServer.start()

  def await(): Unit = tcpServer.await()

  private def consumeRequest(request: HTTPRequest): (Array[Byte], Boolean) = {
    val rawResponse = handler.apply(request)
    rawResponse.headers.put("content-length", rawResponse.body.getBytes.length.toString)
    val keepAlive = !shouldClose(rawResponse)
    if(keepAlive) {
      println("Keeping socket open")
    }
    return (serializeResponse(rawResponse), !keepAlive)
  }

  private def shouldClose(req: HTTPResponse, header: String = "Connection"): Boolean =
    req.headers.getOrElse(header, () => "close").equals("close")

  private def serializeResponse(httpResponse: HTTPResponse): Array[Byte] = {
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

  object StreamState extends Enumeration {
    type State = Value
    val GatheringHeader, GatheringBody, Complete = Value
  }

  class StreamProcessor(consumeRequest: HTTPRequest => (Array[Byte], Boolean)) {

    var totalBytesRead = 0

    var bodyLengthTarget: Option[Int] = None

    var workingRequest: Option[HTTPRequest] = None

    var state: StreamState.State = StreamState.GatheringHeader

    def consume(buffers: ArrayBuffer[ByteBuffer], newRead: Int): Option[(ByteBuffer, Boolean)] = {
      var readBytes = newRead

      if(state == StreamState.GatheringHeader) {
        val completed = headerProcessor(buffers, readBytes)
        if(completed) {
          readBytes = 0
          state = StreamState.GatheringBody
        } else {
          return None
        }
      }

      if(state == StreamState.GatheringBody) {
        val body = bodyProcessor(buffers, readBytes)
        if(body.isDefined) {
          workingRequest.get.body = RawBody(new String(body.get, HTTPServer.UTF8))
          val (data, close) = consumeRequest.apply(workingRequest.get)
          state = StreamState.Complete
          return Some(ByteBuffer.wrap(data), close)
        }
      }
      return None
    }

    private def headerProcessor(buffers: ArrayBuffer[ByteBuffer], i: Int): Boolean = {
      val bufferEnd = buffers.last.position()
      val header = headerOption(buffers, i)
      if(header.isEmpty) {
        // header not complete
        return false
      }
      // Build working request and get body length
      workingRequest = Some(parseHeader(header.get))
      bodyLengthTarget = Some(workingRequest.get.headers.getOrElse("content-length", "0").toInt)

      // Erase header from buffer so body can be placed there
      val trailingBytes = bufferEnd - buffers.last.position()
      removeHeaderFromBuffers(buffers, trailingBytes)
      totalBytesRead = trailingBytes

      // header selection is complete
      return true
    }

    private def removeHeaderFromBuffers(buffers: ArrayBuffer[ByteBuffer], bodyByteCount: Int): Unit = {
      val bytesToCopy = buffers.last
      buffers.clear()
      buffers.append(ByteBuffer.allocate(bytesToCopy.capacity()))
      while(bytesToCopy.hasRemaining) {
        buffers.last.put(bytesToCopy.get())
      }
    }

    private def headerOption(buffer: ArrayBuffer[ByteBuffer], newBytes: Int): Option[String] = {
      totalBytesRead += newBytes
      // quit early if we don't have enough chars to contain our seq
      if (newBytes < 4 && buffer.size <= 1) {
        return None
      }

      // byte count already in buffer
      val existingValues = buffer.last.position() - newBytes

      // if we need to check the previous buffer, do that and save the pattern state
      val prevBufferPatternState = if (buffer.size > 1 || existingValues > 4) {
        val prevBuff = buffer.apply(buffer.size - 2)
        prevBuff.position(prevBuff.capacity() - 4)
        checkBuffer(prevBuff, 0)
      } else {
        0
      }
      // reset the newest buffer position to a location 4 chars before the start of new bytes
      buffer.last.position(if(existingValues > 4) existingValues else 0)
      val latestBufferPatternState = checkBuffer(buffer.last, prevBufferPatternState)

      // if a pattern match was found, stitch together the buffers for parsing
      if(latestBufferPatternState >= 4) {
        val stringData = new String(joinBuffer(buffer, totalBytesRead), HTTPServer.UTF8)
        return Some(stringData)
      }

      return None
    }

    private def joinBuffer(arrayBuffer: ArrayBuffer[ByteBuffer], totalSize: Int): Array[Byte] =
      arrayBuffer.flatMap { e => e.array() }.toArray

    private def bodyProcessor(buffer: ArrayBuffer[ByteBuffer], newBytes: Int): Option[Array[Byte]] = {
      // count bytes until we have received the full body
      totalBytesRead += newBytes
      if(totalBytesRead >= bodyLengthTarget.get) {
        var bodyData = joinBuffer(buffer, totalBytesRead)
        bodyData = bodyData.dropRight(bodyData.length - bodyLengthTarget.get)
        return Some(bodyData)
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
          (name.toLowerCase, value)
        }.toMap

      return HTTPRequest(method, path, httpVersion, headers, urlParams, RawBody(""))
    }
  }
}
