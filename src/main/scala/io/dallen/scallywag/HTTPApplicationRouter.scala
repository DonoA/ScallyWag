package io.dallen.scallywag

import io.dallen.scallywag.HTTPApplicationServer.Response
import io.dallen.scallywag.httpserver.{HTTPMethod, HTTPRequest, HTTPResponseCode}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object HTTPRouter {
  type RouteHandler = (HTTPRequest, Map[String, String], Response) => Unit

  private def buildRouteHandler(httpHandler: HTTPApplicationServer.HTTPHandler): RouteHandler = {
    return (httpRequest: HTTPRequest, params: Map[String, String], response: Response) => {
      httpHandler.apply(new HTTPApplicationServer.Request(httpRequest, params), response)
    }
  }
}

class HTTPRouter {

//  private val customRoutes = new util.ArrayList[(String => Boolean, HTTPApplicationServer.HTTPHandler)]()

  var defaultRoute: HTTPRouter.RouteHandler =
    HTTPRouter.buildRouteHandler((_: HTTPApplicationServer.Request, res: HTTPApplicationServer.Response) =>
      res.code = HTTPResponseCode.NOT_FOUND)

  private val routeTable = Map[HTTPMethod, Router](
    HTTPMethod.GET -> Router(new ListBuffer[(String, List[HTTPRouter.RouteHandler])]()),
    HTTPMethod.POST -> Router(new ListBuffer[(String, List[HTTPRouter.RouteHandler])]()),
  )


  def get(path: String, handlers: AnyRef*): HTTPRouter = {
    registerRoute(HTTPMethod.GET, path, handlers.map {
      case handler: HTTPApplicationServer.HTTPHandler => HTTPRouter.buildRouteHandler(handler)
      case subrouter: HTTPRouter => subrouter.getInternalMethodRouter(HTTPMethod.GET)
      case _ => throw new UnsupportedOperationException("Handlers must be of type HTTPHandler or HTTPRouter!")
    }.toList)
    return this
  }

  def post(path: String, handlers: AnyRef*): HTTPRouter = {
    registerRoute(HTTPMethod.POST, path, handlers.flatMap {
      case handler: HTTPApplicationServer.HTTPHandler => Array(HTTPRouter.buildRouteHandler(handler))
      case subrouter: HTTPRouter => subrouter.routeTable(HTTPMethod.POST).routeTable
      case _ => throw new UnsupportedOperationException("Handlers must be of type HTTPHandler or HTTPRouter!")
    }.toList)
    return this
  }

  private def registerRoute(httpMethod: HTTPMethod, routePattern: String, handler: List[HTTPRouter.RouteHandler]): Unit = {
    routeTable(httpMethod).registerRoute(routePattern, handler)
  }

  def getMethodRouter(httpMethod: HTTPMethod): HTTPRequest => Response =
    routeTable(httpMethod).routeRequest

  private def getInternalMethodRouter(httpMethod: HTTPMethod): HTTPRouter.RouteHandler =
    routeTable(httpMethod).internalRouteRequest

  private case class Router(routeTable: ListBuffer[(String, List[HTTPRouter.RouteHandler])]) {
    def registerRoute(routePattern: String, handler: List[HTTPRouter.RouteHandler]): Unit = {
      routeTable += ((routePattern, handler))
    }

    def routeRequest(httpRequest: HTTPRequest): Response = {
      val response = new Response(httpRequest.headers)
      internalRouteRequest(httpRequest, Map[String, String](), response)
      return response
    }

    def internalRouteRequest(httpRequest: HTTPRequest, params: Map[String, String], response: Response): Unit = {
      routeTable.toList
        .map(route => (patternMatches(httpRequest.location, route._1), route._2))
        .find(route => route._1.isDefined)
        .foreach(route => route._2
          .foreach(routeExecutor => routeExecutor.apply(httpRequest, route._1.get ++ params, response)))
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
