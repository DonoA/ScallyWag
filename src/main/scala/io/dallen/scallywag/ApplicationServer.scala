package io.dallen.scallywag

import io.dallen.scallywag.httpserver._

import scala.collection.mutable

object ApplicationServer {
  type Handler = (ApplicationServer.Request, ApplicationServer.Response) => Unit

  class Request(private val httpRequest: HTTPRequest, val routeParameters: Map[String, String]) {
    private val parent = httpRequest

    def method: String = parent.method
    def location: String = parent.location
    def proto: String = parent.proto
    def headers: Map[String, String] = parent.headers
    def urlParameters: Map[String, String] = parent.urlParameters
    def body: String = parent.body
  }

  class Response(private val defaultHeaders: Map[String, String]) {
    var code: HTTPResponseCode = HTTPResponseCode.OK
    var headers: mutable.Map[String, String] = defaultHeaders
        .filter(p => Array("Connection").contains(p._1))
        .foldLeft(new mutable.HashMap[String, String]())((map, elem) => {
          map += elem
        })
    var body: String = ""
  }
}

class ApplicationServer(port: Int) {

  private val httpServer = new HTTPServer(port, handle)

  var router = new Router()

  def get(path: String, handlers: AnyRef*): ApplicationServer = {
    router.get(path, handlers.toList)
    return this
  }

  def post(path: String, handlers: AnyRef*): ApplicationServer = {
    router.post(path, handlers.toList)
    return this
  }

  def start(): Unit = {
    httpServer.start()
  }

  private def handle(httpRequest: HTTPRequest): HTTPResponse = {
    println(s"Request to ${httpRequest.location}")

    val resp = router.route(HTTPMethod.getByName(httpRequest.method), httpRequest)
    return HTTPResponse(resp.code, resp.headers, resp.body)
  }
}
