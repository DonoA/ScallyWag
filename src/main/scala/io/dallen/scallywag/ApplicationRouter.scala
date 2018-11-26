package io.dallen.scallywag

import io.dallen.scallywag
import io.dallen.scallywag.HTTPApplicationServer.{Request, Response}
import io.dallen.scallywag.httpserver.{HTTPMethod, HTTPRequest, HTTPResponseCode}

object NotFoundRoute {
  def notFound(req: HTTPApplicationServer.Request, res: HTTPApplicationServer.Response): Unit = {
    res.body = "Not Found"
    res.code = HTTPResponseCode.OK
  }
}

import scala.collection.mutable

class ApplicationRouter {

  private val routes = new mutable.HashMap[String, HTTPApplicationServer.HTTPHandler]()

//  private val customRoutes = new util.ArrayList[(String => Boolean, HTTPApplicationServer.HTTPHandler)]()

  var defaultRoute: HTTPApplicationServer.HTTPHandler = NotFoundRoute.notFound

  private var fastRoutes = true

  def addRoute(routePattern: String, handler: HTTPApplicationServer.HTTPHandler): Unit = {
    // detect if route can be fast matched
    routes.put(routePattern, handler)
  }

  def routeRequest(httpRequest: HTTPRequest, response: Response = new Response()): Response = if (fastRoutes) {
    routes
      .getOrElse(httpRequest.location, defaultRoute)
      .apply(new scallywag.HTTPApplicationServer.Request(httpRequest, Map[String, String]()), response)
    return response
  } else {
    routes.toList
      .map(route => (patternMatches(route._1, httpRequest.location), route._2))
      .filter(route => route._1.isDefined)
      .foreach(route => route._2.apply(new scallywag.HTTPApplicationServer.Request(httpRequest, route._1.get), response))
    return response
  }

  private def patternMatches(test: String, pattern: String): Option[Map[String, String]] = {
    return Option.empty
  }

}
