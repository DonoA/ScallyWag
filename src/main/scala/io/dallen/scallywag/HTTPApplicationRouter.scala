package io.dallen.scallywag

import io.dallen.scallywag
import io.dallen.scallywag.HTTPApplicationServer.Response
import io.dallen.scallywag.httpserver.{HTTPMethod, HTTPRequest, HTTPResponseCode}

import scala.collection.mutable

trait ApplicationRouter {
  def routeRequest(httpMethod: HTTPMethod, httpRequest: HTTPRequest): Response
  def registerRoute(httpMethod: HTTPMethod, routePattern: String, handler: HTTPApplicationServer.HTTPHandler): Unit
}

class HTTPApplicationRouter extends ApplicationRouter {

//  private val customRoutes = new util.ArrayList[(String => Boolean, HTTPApplicationServer.HTTPHandler)]()

  var defaultRoute: HTTPApplicationServer.HTTPHandler =
    (_: HTTPApplicationServer.Request, res: HTTPApplicationServer.Response) =>
      res.code = HTTPResponseCode.NOT_FOUND

  private val routeTable = Map[HTTPMethod, Router](
    HTTPMethod.GET -> Router(new mutable.HashMap[String, HTTPApplicationServer.HTTPHandler]()),
    HTTPMethod.POST -> Router(new mutable.HashMap[String, HTTPApplicationServer.HTTPHandler]()),
  )

  override def registerRoute(httpMethod: HTTPMethod, routePattern: String, handler: HTTPApplicationServer.HTTPHandler): Unit =
    routeTable(httpMethod).registerRoute(routePattern, handler)

  override def routeRequest(httpMethod: HTTPMethod, httpRequest: HTTPRequest): Response =
    routeTable(httpMethod).routeRequest(httpRequest)

  private case class Router(routeTable: mutable.Map[String, HTTPApplicationServer.HTTPHandler],
                            var fastRoute: Boolean = false) {
    def registerRoute(routePattern: String, handler: HTTPApplicationServer.HTTPHandler): Unit = {
      if(fastRoute && routePattern.contains(":")) {
        fastRoute = false
      }
      routeTable.put(routePattern, handler)
    }

    def routeRequest(httpRequest: HTTPRequest): Response = {
      val response = new Response(httpRequest.headers)
      if (fastRoute) {
        routeTable
          .getOrElse(httpRequest.location, defaultRoute)
          .apply(new scallywag.HTTPApplicationServer.Request(httpRequest, Map[String, String]()), response)
        return response
      } else {
        routeTable.toList
          .map(route => (patternMatches(httpRequest.location, route._1), route._2))
          .filter(route => route._1.isDefined)
          .foreach(route => route._2.apply(new scallywag.HTTPApplicationServer.Request(httpRequest, route._1.get), response))
        return response
      }
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
