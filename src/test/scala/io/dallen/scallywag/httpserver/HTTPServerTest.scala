package io.dallen.scallywag.httpserver

import org.scalatest._

import scala.collection.mutable

class HTTPServerTest extends FlatSpec {

  it should "calculate response content length" in {
    val data = "HelloWorld"
    val server = new HTTPServer(3000, (_: HTTPServer.Request) =>
      HTTPServer.Response(HTTPServer.ResponseCode.OK, mutable.Map[String, String](), data))
    val consumeRequest = server.getClass.getDeclaredMethod("consumeRequest", classOf[HTTPServer.Request])
    consumeRequest.setAccessible(true)

    val (rawResponse, _) = consumeRequest.invoke(server, null).asInstanceOf[(Array[Byte], Boolean)]
    val response = new String(rawResponse)
    assert(response.contains("content-length: 10"))
  }

  it should "serialize http responses to string" in {
    val server = new HTTPServer(3000, (_: HTTPServer.Request) =>
      HTTPServer.Response(
        HTTPServer.ResponseCode.NOT_FOUND,
        mutable.Map[String, String]("sample-header" -> "headervalue"),
        "bodydata"))
    val consumeRequest = server.getClass.getDeclaredMethod("consumeRequest", classOf[HTTPServer.Request])
    consumeRequest.setAccessible(true)

    val (rawResponse, _) = consumeRequest.invoke(server, null).asInstanceOf[(Array[Byte], Boolean)]
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
    val server = new HTTPServer(3000, (_: HTTPServer.Request) =>
      HTTPServer.Response(HTTPServer.ResponseCode.OK, mutable.Map[String, String]("connection" -> "keep-alive"), ""))
    val consumeRequest = server.getClass.getDeclaredMethod("consumeRequest", classOf[HTTPServer.Request])
    consumeRequest.setAccessible(true)

    val (_, close) = consumeRequest.invoke(server, null).asInstanceOf[(Array[Byte], Boolean)]
    assert(!close)
  }
}
