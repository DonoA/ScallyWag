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

  class Response() {
    var code: HTTPResponseCode = HTTPResponseCode.OK
    var headers: mutable.Map[String, String] = new mutable.HashMap[String, String]()
    var body: String = ""
  }

}

class HTTPApplicationServer(port: Int) {

  private val httpServer = new HTTPServer(port, handle)

  private val routerByMethod = Map[HTTPMethod, ApplicationRouter](
    HTTPMethod.GET -> new ApplicationRouter(),
    HTTPMethod.POST -> new ApplicationRouter(),
  )

  def get(path: String, handler: HTTPApplicationServer.HTTPHandler): HTTPApplicationServer = {
    routerByMethod(HTTPMethod.GET).addRoute(path, handler)
    return this
  }

  def post(path: String, handler: HTTPApplicationServer.HTTPHandler): HTTPApplicationServer = {
    routerByMethod(HTTPMethod.POST).addRoute(path, handler)
    return this
  }

  def start(): Unit = {
    httpServer.start()
  }

  private def handle(httpRequest: HTTPRequest): HTTPResponse = {
    println(httpRequest)
    val resp = routerByMethod(HTTPMethod.getByName(httpRequest.method)).routeRequest(httpRequest)
    return HTTPResponse(resp.code, resp.headers.toMap, resp.body)
  }
}
