package io.dallen.scallywag.httpserver

import java.nio.ByteBuffer

import io.dallen.scallywag.httpserver.HTTPServer.Request
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class TCPStreamCollectorTest extends FlatSpec with Matchers {

  it should "Handle empty read cycles" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringHeader)
    val buffers = new ArrayBuffer[ByteBuffer]()
    collector.consume(buffers, 0)
    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals 0)
  }

  it should "Handle any sized ByteBuffer" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringHeader)
    val buffers = new ArrayBuffer[ByteBuffer]()
    val data = "HelloWorld"
    buffers.append(ByteBuffer.allocate(16))
    buffers.last.put(data.getBytes("utf-8"))
    collector.consume(buffers, data.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals data.length)

    buffers.append(ByteBuffer.allocate(1024))
    0 to 10 foreach { _ => buffers.last.put(data.getBytes("utf-8")) }
    collector.consume(buffers, data.length * 10)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals data.length * 11)
  }

  it should "Only change from GatheringHeader state when \\r\\n\\r\\n encountered" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringHeader)

    val noEndData = "GET /index.html HTTP/1.1\r\ncontent-length: 1"
    val endChars = "\r\n\r\n"
    val completeDat = noEndData + endChars

    val buffers = new ArrayBuffer[ByteBuffer]()
    buffers.append(ByteBuffer.allocate(128))
    buffers.last.put(noEndData.getBytes("utf-8"))
    collector.consume(buffers, noEndData.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals noEndData.length)

    buffers.last.put(completeDat.getBytes("utf-8"))
    collector.consume(buffers, completeDat.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringBody)
    assert(collector.totalBytesRead equals 0)
  }

  it should "Change from GatheringHeader state when \\r\\n\\r\\n encountered in several reads" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringHeader)

    val noEndData = "GET /index.html HTTP/1.1\r\ncontent-length: 1\r\n"
    val endChars = "\r\n"

    val buffers = new ArrayBuffer[ByteBuffer]()
    buffers.append(ByteBuffer.allocate(128))
    buffers.last.put(noEndData.getBytes("utf-8"))
    collector.consume(buffers, noEndData.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals noEndData.length)

    buffers.last.put(endChars.getBytes("utf-8"))
    collector.consume(buffers, endChars.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringBody)
    assert(collector.totalBytesRead equals 0)
    assert(collector.bodyLengthTarget.isDefined)
    assert(collector.bodyLengthTarget.get equals 1)
  }

  it should "Change from GatheringHeader state when \\r\\n\\r\\n encountered in several buffers" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringHeader)

    val noEndData = "GET /index.html HTTP/1.1\r\ncontent-length: 1\r\n"
    val endChars = "\r\n"

    val buffers = new ArrayBuffer[ByteBuffer]()
    buffers.append(ByteBuffer.allocate(45))
    buffers.last.put(noEndData.getBytes("utf-8"))
    collector.consume(buffers, noEndData.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals noEndData.length)

    buffers.append(ByteBuffer.allocate(45))
    buffers.last.put(endChars.getBytes("utf-8"))
    collector.consume(buffers, endChars.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringBody)
    assert(collector.totalBytesRead equals 0)
    assert(collector.bodyLengthTarget.isDefined)
    assert(collector.bodyLengthTarget.get equals 1)
  }

  it should "keep data after \\r\\n\\r\\n is encountered" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringHeader)

    val noEndData = "GET /index.html HTTP/1.1\r\ncontent-length: 10\r\n"
    val extraChars = "extradata"
    val endChars = "\r\n" + extraChars


    val buffers = new ArrayBuffer[ByteBuffer]()
    buffers.append(ByteBuffer.allocate(noEndData.length))
    buffers.last.put(noEndData.getBytes("utf-8"))
    collector.consume(buffers, noEndData.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals noEndData.length)

    buffers.append(ByteBuffer.allocate(16))
    buffers.last.put(endChars.getBytes("utf-8"))
    collector.consume(buffers, endChars.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringBody)
    assert(collector.totalBytesRead equals 9)
    assert(collector.bodyLengthTarget.isDefined)
    assert(collector.bodyLengthTarget.get equals 10)

    val data = buffers.map(bb => new String(bb.array(), "utf-8"))
    assert(data.mkString("").replace("\0", "") equals extraChars)
  }

  it should "assume content length if none is given" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringHeader)

    val noEndData = "GET /index.html HTTP/1.1\r\n\r\n"

    val buffers = new ArrayBuffer[ByteBuffer]()
    buffers.append(ByteBuffer.allocate(noEndData.length))
    buffers.last.put(noEndData.getBytes("utf-8"))
    collector.consume(buffers, noEndData.length)

    assert(collector.state equals TCPStreamCollector.StreamState.Complete)
    assert(collector.totalBytesRead equals 0)
    assert(collector.bodyLengthTarget.isDefined)
    assert(collector.bodyLengthTarget.get equals 0)
  }

  it should "change from GatheringBody when target body length has been reached" in {
    val collector = new TCPStreamCollector(_ => (Array[Byte](), true), TCPStreamCollector.StreamState.GatheringBody)
    collector.bodyLengthTarget = Some(10)
    collector.workingRequest = Some(Request("GET", "/index.html", "1.1", Map(), Map(), RawBody("")))
    val hello = "hello"
    val world = "world"
    val cutOff = "extras"

    val buffers = new ArrayBuffer[ByteBuffer]()

    buffers.append(ByteBuffer.allocate(hello.length + world.length + cutOff.length))
    buffers.last.put(hello.getBytes("utf-8"))
    collector.consume(buffers, hello.length)

    assert(collector.state equals TCPStreamCollector.StreamState.GatheringBody)
    assert(collector.totalBytesRead equals hello.length)

    buffers.last.put(world.getBytes("utf-8"))
    buffers.last.put(cutOff.getBytes("utf-8"))
    collector.consume(buffers, world.length + cutOff.length)

    assert(collector.state equals TCPStreamCollector.StreamState.Complete)
    assert(collector.totalBytesRead.equals(hello.length + world.length + cutOff.length))
    assert(collector.workingRequest.get.body.asInstanceOf[RawBody].data.equals(hello + world))
  }

  it should "reset if socket is not closed" in {
    var savedRequest: Request = null

    val collector = new TCPStreamCollector({ req: Request =>
      savedRequest = req
      (Array[Byte](), false)
    }, TCPStreamCollector.StreamState.GatheringHeader)

    val noEndData = "GET /index.html HTTP/1.1\r\n\r\n"

    val buffers = new ArrayBuffer[ByteBuffer]()
    buffers.append(ByteBuffer.allocate(noEndData.length))
    buffers.last.put(noEndData.getBytes("utf-8"))
    collector.consume(buffers, noEndData.length)

    assert(savedRequest.location.equals("/index.html"))
    assert(collector.state equals TCPStreamCollector.StreamState.GatheringHeader)
    assert(collector.totalBytesRead equals 0)
    assert(collector.bodyLengthTarget.isEmpty)
    assert(collector.workingRequest.isEmpty)
  }

  it should "throw IllegalArgumentException if a malformed HTTP request is passed in" in {

    val collector = new TCPStreamCollector({ req: Request => (Array[Byte](), true)},
      TCPStreamCollector.StreamState.GatheringHeader)

    val requestData = "this is not an http header\r\n\r\n"

    val buffers = new ArrayBuffer[ByteBuffer]()
    buffers.append(ByteBuffer.allocate(requestData.length))
    buffers.last.put(requestData.getBytes("utf-8"))

    a [IllegalArgumentException] should be thrownBy {
      collector.consume(buffers, requestData.length)
    }
  }
}
