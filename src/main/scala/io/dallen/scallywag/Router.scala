package io.dallen.scallywag

import io.dallen.scallywag
import io.dallen.scallywag.ApplicationServer.Response
import io.dallen.scallywag.httpserver.{HTTPMethod, HTTPRequest, HTTPResponseCode}

import scala.collection.mutable

class Router {

//  private val customRoutes = new util.ArrayList[(String => Boolean, HTTPApplicationServer.HTTPHandler)]()

  var defaultRoute: ApplicationServer.Handler =
    (_: ApplicationServer.Request, res: ApplicationServer.Response) =>
      res.code = HTTPResponseCode.NOT_FOUND

  val routeTable: Map[HTTPMethod, SimpleRouter] = Map[HTTPMethod, SimpleRouter](
    HTTPMethod.GET -> SimpleRouter(new mutable.HashMap[String, List[ApplicationServer.Handler]]()),
    HTTPMethod.POST -> SimpleRouter(new mutable.HashMap[String, List[ApplicationServer.Handler]]()),
  )


  def get(path: String, handlers: AnyRef*): Router = {
    registerRoutes(HTTPMethod.GET, path, flattenVarargs(handlers))
    return this
  }

  def post(path: String, handlers: AnyRef*): Router = {
    registerRoutes(HTTPMethod.POST, path, flattenVarargs(handlers))
    return this
  }

  private def flattenVarargs(varargs: Seq[AnyRef]): List[AnyRef] = varargs.head match {
    case r: List[AnyRef] => r
    case r: AnyRef => List(r)
  }

  private def registerRoutes(httpMethod: HTTPMethod, routePattern: String, handlers: List[AnyRef]): Unit = {
    // Register simple routes
    routeTable(httpMethod).registerRoute(routePattern, handlers
      .filter(_.isInstanceOf[ApplicationServer.Handler])
      .map(_.asInstanceOf[ApplicationServer.Handler]))

    // flatten routes
    handlers
      .filter(_.isInstanceOf[Router])
      .map(_.asInstanceOf[Router])
      .flatMap { subrouter => subrouter.routeTable(httpMethod).routeTable.toList }
      .foreach { case (subpath, subhandlers) => routeTable(httpMethod).registerRoute(routePattern + subpath, subhandlers) }
  }

  def route(httpMethod: HTTPMethod, httpRequest: HTTPRequest): ApplicationServer.Response =
    routeTable(httpMethod).routeRequest(httpRequest)

  case class SimpleRouter(routeTable: mutable.HashMap[String, List[ApplicationServer.Handler]],
                            var fastRoute: Boolean = true) {
    def registerRoute(routePattern: String, handler: List[ApplicationServer.Handler]): Unit = if (handler.nonEmpty) {
      if(routePattern.contains(":")) {
        fastRoute = false
      }
      routeTable += (routePattern -> handler)
    }

    def routeRequest(httpRequest: HTTPRequest): Response = {
      val response = new Response(httpRequest.headers)
      internalRouteRequest(httpRequest, response)
      return response
    }

    def internalRouteRequest(httpRequest: HTTPRequest, response: Response): Unit = {
      routeTable.toList
        .map { case (pattern, handlers) => (patternMatches(httpRequest.location, pattern), handlers) }
        .find { case (route, handlers) => route.isDefined }
        .foreach { case (route, handlers) => handlers
          .foreach { routeExecutor => routeExecutor.apply(
            new scallywag.ApplicationServer.Request(httpRequest, route.get), response) }}
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
