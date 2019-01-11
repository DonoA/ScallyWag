package io.dallen.scallywag.appserver

import io.dallen.scallywag.httpserver.HTTPServer.Method
import io.dallen.scallywag.httpserver._

import scala.collection.mutable
import scala.concurrent.Future

object ApplicationServer {
  type Handler = (ApplicationServer.Request, ApplicationServer.Response) => Unit

  class Request(private val httpRequest: HTTPServer.Request, val routeParameters: Map[String, String]) {
    private val parent = httpRequest
    private val context = new mutable.HashMap[String, AnyRef]()
    private val cookies = new mutable.HashMap[String, String]()
    parent
      .headers
      .get("cookie")
      .foreach(part =>
        part
          .split("; ")
          .map(cookie => cookie.split("="))
          .foreach { case Array(name, value) => cookies.put(name, value) })


    lazy val keepAlive: Boolean = parent.headers.getOrElse[String]("connection", "close").equals("keep-alive")
    lazy val host: String = parent.headers.getOrElse("host", "")
    lazy val cacheEnabled: Boolean = !parent.headers.getOrElse[String]("cache-control", "no-cache").equals("no-cache")
    lazy val contentType: String = parent.headers.getOrElse("content-type", "text/plain")
    lazy val accept: String = parent.headers.getOrElse("accept", "*/*")

    def method: HTTPServer.Method = parent.method
    def location: String = parent.location
    def proto: String = parent.proto
    def headers: Map[String, String] = parent.headers
    def urlParameters: Map[String, String] = parent.urlParameters
    def body: RequestBody = parent.body
    def addContext(name: String, data: AnyRef): Unit = context.put(name, data)
    def getContext(name: String): Option[AnyRef] = context.get(name)
    def cookie(name: String): Option[String] = cookies.get(name)
  }

  case class ResponseCookie(value: String, domain: String, path: String = "/",
                            expires: Long = System.currentTimeMillis() + (1000 * 60 * 60 * 24 * 365))

  class Response(private val defaultHeaders: Map[String, String]) {
    var code: HTTPServer.ResponseCode = HTTPServer.ResponseCode.OK
    var headers: mutable.Map[String, String] = new mutable.HashMap()
    var body: String = ""

    private val cookies = new mutable.HashMap[String, ResponseCookie]()

    private var shouldClose = !defaultHeaders.getOrElse[String]("connection", "close").equals("keep-alive")

    def setClose(newClose: Boolean): Unit = this.shouldClose = newClose
    def close: Boolean = this.shouldClose
    def setCookie(name: String, value: String, domain: String): Unit = cookies.put(name, ResponseCookie(value, domain))
    def setCookie(name: String, value: ResponseCookie): Unit = cookies.put(name, value)
    def allCookie: mutable.HashMap[String, ResponseCookie] = cookies
  }
}

class ApplicationServer(port: Int) {

  private val httpServer = new HTTPServer(port, handle)

  var router = new Router()

  def getPort: Int = port

  def get(path: String, handlers: ApplicationServer.Handler*): ApplicationServer = {
    router.get(path, handlers.toList)
    return this
  }

  def post(path: String, handlers: ApplicationServer.Handler*): ApplicationServer = {
    router.post(path, handlers.toList)
    return this
  }

  def use(path: String, handlers: AnyRef*): ApplicationServer = {
    router.use(path, handlers)
    return this
  }

  def use(handlers: ApplicationServer.Handler*): ApplicationServer = {
    router.use(handlers.toList)
    return this
  }

  def start(): Future[Unit] = httpServer.start()

  def stop(): Unit = httpServer.stop()

  def getRouteMap: String =
    router.routeTable.map { case (Method(method), simpleRouter) =>
      val routeList = simpleRouter.routeList.map { case (path, handlerList) =>
        val handlerChain = handlerList.map { _.toString() }.mkString("->")
        s"\t$path => $handlerChain"
      }.mkString("\n")
      s"$method:\n$routeList"
    }.mkString("\n\n")



  private def handle(httpRequest: HTTPServer.Request): HTTPServer.Response = {
    println(s"Request to ${httpRequest.location}")

    val resp = router.route(httpRequest.method, httpRequest)

    resp.headers.put("connection", if(resp.close) "close" else "keep-alive")
    return HTTPServer.Response(resp.code, resp.headers, resp.body)
  }
}
