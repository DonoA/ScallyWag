package io.dallen.scallywag.appserver

import io.dallen.scallywag.httpserver.HTTPServer

import scala.collection.mutable

class Router {

  var defaultRoute: ApplicationServer.Handler =
    (_: ApplicationServer.Request, res: ApplicationServer.Response) =>
      res.code = HTTPServer.ResponseCode.NOT_FOUND

  val routeTable: Map[HTTPServer.Method, SimpleRouter] = Map[HTTPServer.Method, SimpleRouter](
    HTTPServer.Method.GET -> SimpleRouter(new mutable.HashMap[String, List[ApplicationServer.Handler]]()),
    HTTPServer.Method.POST -> SimpleRouter(new mutable.HashMap[String, List[ApplicationServer.Handler]]()),
  )


  def get(path: String, handlers: AnyRef*): Router = {
    registerRoutes(HTTPServer.Method.GET, path, flattenVarargs(handlers))
    return this
  }

  def post(path: String, handlers: AnyRef*): Router = {
    registerRoutes(HTTPServer.Method.POST, path, flattenVarargs(handlers))
    return this
  }

  private def flattenVarargs(varargs: Seq[AnyRef]): List[AnyRef] = varargs.head match {
    case r: List[AnyRef] => r
    case r: AnyRef => List(r)
  }

  private def registerRoutes(httpMethod: HTTPServer.Method, routePattern: String, handlers: List[AnyRef]): Unit = {
    // Register simple routes
    routeTable(httpMethod).registerRoute(routePattern, handlers
      .filter(_.isInstanceOf[ApplicationServer.Handler])
      .map(_.asInstanceOf[ApplicationServer.Handler]))

    // flatten routes from subrouters before commiting them
    handlers
      .filter(_.isInstanceOf[Router])
      .map(_.asInstanceOf[Router])
      .flatMap { subrouter => subrouter.routeTable(httpMethod).routeTable.toList }
      .foreach { case (subpath, subhandlers) => routeTable(httpMethod).registerRoute(routePattern + subpath, subhandlers) }
  }

  def route(httpMethod: HTTPServer.Method, httpRequest: HTTPServer.Request): ApplicationServer.Response =
    routeTable(httpMethod).routeRequest(httpRequest)

  case class SimpleRouter(routeTable: mutable.HashMap[String, List[ApplicationServer.Handler]],
                            var fastRoute: Boolean = true) {
    def registerRoute(routePattern: String, handler: List[ApplicationServer.Handler]): Unit = if (handler.nonEmpty) {
      if(routePattern.contains(":")) {
        fastRoute = false
      }
      routeTable += (routePattern -> handler)
    }

    def routeRequest(httpRequest: HTTPServer.Request): ApplicationServer.Response = {
      val response = new ApplicationServer.Response(httpRequest.headers)
      internalRouteRequest(httpRequest, response)
      return response
    }

    def internalRouteRequest(httpRequest: HTTPServer.Request, response: ApplicationServer.Response): Unit = {
      routeTable.toList
        .map { case (pattern, handlers) => (patternMatches(httpRequest.location, pattern), handlers) }
        .find { case (route, handlers) => route.isDefined }
        .foreach { case (route, handlers) => handlers
          .foreach { routeExecutor => routeExecutor.apply(
            new ApplicationServer.Request(httpRequest, route.get), response) }}
    }

    private def patternMatches(test: String, pattern: String): Option[Map[String, String]] = {
      val testSegments = test.split("/")
      val patternSegments = pattern.split("/")
      if (testSegments.length != patternSegments.length) {
        return Option.empty
      }

      val params = new mutable.HashMap[String, String]()

      val testIterator = testSegments.iterator
      val patternIterator = patternSegments.iterator

      while (testIterator.hasNext) {
        val testBit = testIterator.next()
        val patternBit = patternIterator.next()
        if (patternBit.startsWith(":")) {
          params.put(patternBit.drop(1), testBit)
        } else if (!testBit.equals(patternBit)) {
          return Option.empty
        }
      }

      return Option(params.toMap)
    }
  }

}
