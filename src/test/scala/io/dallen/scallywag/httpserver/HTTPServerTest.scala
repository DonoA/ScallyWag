package io.dallen.scallywag.httpserver

import java.net.URL
import java.util.Scanner

import org.mockito.Mockito
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.scalatest._

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class HTTPServerTest extends FlatSpec {

  it should "calculate response content length" in {
    val mockRawRequest = Mockito.mock(classOf[TCPStreamCollector.RawRequest])
    when(mockRawRequest.method).thenAnswer((_: InvocationOnMock) => "GET")
    when(mockRawRequest.body).thenAnswer((_: InvocationOnMock) => Array[Byte]())
    val data = "HelloWorld"
    val server = new HTTPServer(3000, (_: HTTPServer.Request) =>
      HTTPServer.Response(HTTPServer.ResponseCode.OK, mutable.Map[String, String](), data))
    val consumeRequest = server.getClass.getDeclaredMethod("consumeRequest", classOf[TCPStreamCollector.RawRequest])
    consumeRequest.setAccessible(true)

    val TCPStreamCollector.RawResponse(rawResponse, _) = consumeRequest.invoke(server, mockRawRequest)
      .asInstanceOf[TCPStreamCollector.RawResponse]
    val response = new String(rawResponse)
    assert(response.contains("content-length: 10"))
  }

  it should "serialize http responses to string" in {
    val mockRawRequest = Mockito.mock(classOf[TCPStreamCollector.RawRequest])
    when(mockRawRequest.method).thenAnswer((_: InvocationOnMock) => "GET")
    when(mockRawRequest.body).thenAnswer((_: InvocationOnMock) => Array[Byte]())
    val server = new HTTPServer(3000, (_: HTTPServer.Request) =>
      HTTPServer.Response(
        HTTPServer.ResponseCode.NOT_FOUND,
        mutable.Map[String, String]("sample-header" -> "headervalue"),
        "bodydata"))
    val consumeRequest = server.getClass.getDeclaredMethod("consumeRequest", classOf[TCPStreamCollector.RawRequest])
    consumeRequest.setAccessible(true)

    val TCPStreamCollector.RawResponse(rawResponse, _) = consumeRequest.invoke(server, mockRawRequest)
      .asInstanceOf[TCPStreamCollector.RawResponse]
    val response = new String(rawResponse)
    val expected =
     """|HTTP/1.1 404 NOT FOUND
        |sample-header: headervalue
        |content-length: 8
        |
        |bodydata""".stripMargin.replace("\n", "\r\n")
    assert(expected equals response)
  }

  it should "respect connection: keep-alive" in {
    val mockRawRequest = Mockito.mock(classOf[TCPStreamCollector.RawRequest])
    when(mockRawRequest.method).thenAnswer((_: InvocationOnMock) => "GET")
    when(mockRawRequest.body).thenAnswer((_: InvocationOnMock) => Array[Byte]())
    val server = new HTTPServer(3000, (_: HTTPServer.Request) =>
      HTTPServer.Response(HTTPServer.ResponseCode.OK, mutable.Map[String, String]("connection" -> "keep-alive"), ""))
    val consumeRequest = server.getClass.getDeclaredMethod("consumeRequest", classOf[TCPStreamCollector.RawRequest])
    consumeRequest.setAccessible(true)

    val TCPStreamCollector.RawResponse(_, close) = consumeRequest.invoke(server, mockRawRequest)
      .asInstanceOf[TCPStreamCollector.RawResponse]
    assert(!close)
  }

  it should "process real http requests" in {
    var savedReq: HTTPServer.Request = null
    var server: HTTPServer = null
    server = new HTTPServer(3000, { req: HTTPServer.Request =>
      server.stop()
      savedReq = req
      HTTPServer.Response(HTTPServer.ResponseCode.OK, mutable.Map[String, String](), "")
    })
    val f = server.start()
    new URL("http://localhost:3000/index?param1&param2=hello").openStream
    Await.result(f, Duration(100, "millis"))

    assert(savedReq.method.name equals "GET")
    assert(savedReq.location equals "/index")
    assert(savedReq.proto equals "1.1")
    assert(savedReq.headers("host") equals "localhost:3000")
    assert(savedReq.urlParameters("param1") equals "true")
    assert(savedReq.urlParameters("param2") equals "hello")
    assert(savedReq.body.isInstanceOf[RawBody])
    assert(savedReq.body.asInstanceOf[RawBody].data.isEmpty)
  }
}
