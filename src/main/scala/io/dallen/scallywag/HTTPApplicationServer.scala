package io.dallen.scallywag

import io.dallen.scallywag.httpserver._

import scala.collection.mutable

object HTTPApplicationServer {
  type HTTPHandler = (HTTPApplicationServer.Request, HTTPApplicationServer.Response) => Unit

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

class HTTPApplicationServer(port: Int) {

  private val httpServer = new HTTPServer(port, handle)

  var router = new HTTPRouter()

  def get(path: String, handler: HTTPApplicationServer.HTTPHandler): HTTPApplicationServer = {
    router.get(path, handler)
    return this
  }

  def get(path: String, subrouter: HTTPRouter): HTTPApplicationServer = {
    router.get(path, subrouter)
    return this
  }

  def post(path: String, handler: HTTPApplicationServer.HTTPHandler): HTTPApplicationServer = {
    router.post(path, handler)
    return this
  }

  def post(path: String, subrouter: HTTPRouter): HTTPApplicationServer = {
    router.post(path, subrouter)
    return this
  }

  def start(): Unit = {
    httpServer.start()
  }

  private def handle(httpRequest: HTTPRequest): HTTPResponse = {
    println(httpRequest)

    val resp = router.getMethodRouter(HTTPMethod.getByName(httpRequest.method)).apply(httpRequest)
    return HTTPResponse(resp.code, resp.headers, resp.body)
  }
}
