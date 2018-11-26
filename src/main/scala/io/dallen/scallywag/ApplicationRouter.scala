package io.dallen.scallywag

import io.dallen.scallywag
import io.dallen.scallywag.HTTPApplicationServer.Response
import io.dallen.scallywag.httpserver.{HTTPRequest, HTTPResponseCode}

import scala.collection.mutable

class ApplicationRouter {

  private val routes = new mutable.HashMap[String, HTTPApplicationServer.HTTPHandler]()

//  private val customRoutes = new util.ArrayList[(String => Boolean, HTTPApplicationServer.HTTPHandler)]()

  var defaultRoute: HTTPApplicationServer.HTTPHandler =
    (_: HTTPApplicationServer.Request, res: HTTPApplicationServer.Response) =>
      res.code = HTTPResponseCode.NOT_FOUND

  private var fastRoutes = true

  def addRoute(routePattern: String, handler: HTTPApplicationServer.HTTPHandler): Unit = {
    if(!fastRoutes && routePattern.contains(":")) {
      fastRoutes = false
    }
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
    val testSegments = test.split("/").iterator
    val patternSegments = pattern.split("/").iterator
    if (testSegments.size != patternSegments.size) {
      return Option.empty
    }

    val params = new mutable.HashMap[String, String]()

    while (testSegments.hasNext) {
      val testBit = testSegments.next()
      val patternBit = patternSegments.next()
      if (patternBit.startsWith(":")) {
        params.put(patternBit.drop(1), testBit)
      } else if (!testBit.equals(patternBit)) {
        return Option.empty
      }
    }

    return Option(params.toMap)
  }

}
