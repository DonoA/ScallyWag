package io.dallen.scallywag.appserver

import io.dallen.scallywag.httpserver._

import scala.collection.mutable
import scala.concurrent.Future

object ApplicationServer {
  type Handler = (ApplicationServer.Request, ApplicationServer.Response) => Unit

  class Request(private val httpRequest: HTTPServer.Request, val routeParameters: Map[String, String]) {
    private val parent = httpRequest

    def method: String = parent.method
    def location: String = parent.location
    def proto: String = parent.proto
    def headers: Map[String, String] = parent.headers
    def urlParameters: Map[String, String] = parent.urlParameters
    def body: RequestBody = parent.body
  }

  class Response(private val defaultHeaders: Map[String, String]) {
    var code: HTTPServer.ResponseCode = HTTPServer.ResponseCode.OK
    var headers: mutable.Map[String, String] = defaultHeaders
        .filter(p => Array("connection").contains(p._1))
        .foldLeft(new mutable.HashMap[String, String]())((map, elem) => {
          map += elem
        })
    var body: String = ""
  }
}

class ApplicationServer(port: Int) {

  private val httpServer = new HTTPServer(port, handle)

  var router = new Router()

  def getPort: Int = port

  def get(path: String, handlers: AnyRef*): ApplicationServer = {
    router.get(path, handlers.toList)
    return this
  }

  def post(path: String, handlers: AnyRef*): ApplicationServer = {
    router.post(path, handlers.toList)
    return this
  }

  def start(): Future[Unit] = httpServer.start()

  def stop(): Unit = httpServer.stop()

  private def handle(httpRequest: HTTPServer.Request): HTTPServer.Response = {
    println(s"Request to ${httpRequest.location}")

    val resp = router.route(HTTPServer.Method.getByName(httpRequest.method), httpRequest)
    return HTTPServer.Response(resp.code, resp.headers, resp.body)
  }
}
