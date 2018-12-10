package io.dallen.scallywag.httpserver

import java.nio.ByteBuffer

import io.dallen.scallywag.httpserver.HTTPServer.Request
import io.dallen.scallywag.httpserver.TCPStreamCollector.StreamState
import io.dallen.scallywag.tcpserver.TCPServer

import scala.collection.mutable.ArrayBuffer

object TCPStreamCollector {
  object StreamState extends Enumeration {
    type State = Value
    val GatheringHeader, GatheringBody, Complete = Value
  }

  case class RawRequest(method: String,
                        location: String,
                        proto: String,
                        headers: Map[String, String],
                        urlParameters: Map[String, String],
                        var body: Array[Byte])

  case class RawResponse(body: Array[Byte], close: Boolean)
}

class TCPStreamCollector(consumeRequest: TCPStreamCollector.RawRequest => TCPStreamCollector.RawResponse,
                         var state: StreamState.State) {

  var totalBytesRead = 0

  var bodyLengthTarget: Option[Int] = None

  var workingRequest: Option[TCPStreamCollector.RawRequest] = None

  def this(consumeRequest: TCPStreamCollector.RawRequest => TCPStreamCollector.RawResponse) {
    this(consumeRequest, StreamState.GatheringHeader)
  }

  def consume(buffers: ArrayBuffer[ByteBuffer], newRead: Int): Option[TCPServer.TCPResponse] = {
    if(newRead <= 0) {
      return None
    }

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
        workingRequest.get.body = body.get
        val TCPStreamCollector.RawResponse(data, close) = consumeRequest.apply(workingRequest.get)
        if(close) {
          state = StreamState.Complete
        } else {
          totalBytesRead = 0
          bodyLengthTarget = None
          workingRequest = None
          buffers.clear()
          state = StreamState.GatheringHeader
        }
        return Some(TCPServer.TCPResponse(ByteBuffer.wrap(data), close))
      }
    }
    return None
  }

  private def headerProcessor(buffers: ArrayBuffer[ByteBuffer], i: Int): Boolean = {
    val bufferEnd = buffers.last.position()
    val header = headerOption(buffers, i, bufferEnd)
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

  private def headerOption(buffer: ArrayBuffer[ByteBuffer], newBytes: Int, lastBuffMaxPos: Int): Option[String] = {
    totalBytesRead += newBytes
    // quit early if we don't have enough chars to contain our seq
    if (buffer.last.position() < 4 && buffer.size <= 1) {
      return None
    }

    // byte count already in buffer
    val existingValues = buffer.last.position() - newBytes

    // if we need to check the previous buffer, do that and save the pattern state
    val prevBufferPatternState = if (buffer.size > 1 && existingValues < 4) {
      val prevBuff = buffer.apply(buffer.size - 2)
      prevBuff.position(prevBuff.capacity() - 4)
      checkBuffer(prevBuff, 0, prevBuff.capacity())
    } else {
      0
    }
    // reset the newest buffer position to a location 4 chars before the start of new bytes
    buffer.last.position(if(existingValues > 4) existingValues - 3 else 0)
    val latestBufferPatternState = checkBuffer(buffer.last, prevBufferPatternState, lastBuffMaxPos)

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

  private def checkBuffer(byteBuffer: ByteBuffer, initPatternMatches: Int, maxPos: Int): Int = {
    var patternMatches = initPatternMatches
    while(byteBuffer.position() < maxPos) {
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

  private def parseHeader(stringMessage: String): TCPStreamCollector.RawRequest = {
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

    val headers = Option(regexMatch
      .group("headers"))
      .map { headers =>
        headers.split("\r\n")
          .map { header =>
            val Array(name, value) = header.split(": ", 2)
            (name.toLowerCase, value)
          }.toMap
      }


    return TCPStreamCollector.RawRequest(method, path, httpVersion, headers.getOrElse(Map[String, String]()),
      urlParams, null)
  }
}
