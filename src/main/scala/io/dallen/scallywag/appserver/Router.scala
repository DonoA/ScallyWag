package io.dallen.scallywag.appserver

import io.dallen.scallywag.httpserver.HTTPServer

import scala.collection.mutable

class Router {

  var defaultRoute: ApplicationServer.Handler =
    (_: ApplicationServer.Request, res: ApplicationServer.Response) =>
      res.code = HTTPServer.ResponseCode.NOT_FOUND

  val routeTable: Map[HTTPServer.Method, SimpleRouter] = Map[HTTPServer.Method, SimpleRouter](
    HTTPServer.Method.ANY -> SimpleRouter(List()),
    HTTPServer.Method.GET -> SimpleRouter(List()),
    HTTPServer.Method.POST -> SimpleRouter(List()),
  )

  var middleWare: List[ApplicationServer.Handler] = List()

  def get(path: String, handlers: ApplicationServer.Handler*): Router = {
    routeTable(HTTPServer.Method.GET).registerRoute(path, handlers.toList)
    return this
  }

  def get(path: String, handlers: List[ApplicationServer.Handler]): Router = {
    routeTable(HTTPServer.Method.GET).registerRoute(path, handlers)
    return this
  }

  def post(path: String, handlers: ApplicationServer.Handler*): Router = {
    routeTable(HTTPServer.Method.POST).registerRoute(path, handlers.toList)
    return this
  }

  def post(path: String, handlers: List[ApplicationServer.Handler]): Router = {
    routeTable(HTTPServer.Method.POST).registerRoute(path, handlers)
    return this
  }

  def use(path: String, handlers: AnyRef*): Router = {
    registerRoutes(path, flattenVarargs(handlers))
    return this
  }

  def use(handlers: ApplicationServer.Handler*): Router = {
    this.middleWare ++= handlers.toList
    return this
  }

  def use(handlers: List[ApplicationServer.Handler]): Router = {
    this.middleWare ++= handlers.toList
    return this
  }

  private def flattenVarargs(varargs: Seq[AnyRef]): List[AnyRef] = varargs.head match {
    case r: mutable.WrappedArray[AnyRef] => r.toList
    case r: AnyRef => List(r)
  }

  private def registerRoutes(routePattern: String, handlers: List[AnyRef]): Unit = {
    // take all functions and apply them as middle ware
    // Take all routers and apply them to the current router with the given middle ware attached in the handler list
    val middleWare = handlers
        .flatMap {
          handl => handl match {
            case router: Router => router.middleWare
            case handlr: ApplicationServer.Handler => List(handlr)
          }
        }

    // flatten routes from subrouters before commiting them
    handlers
      .filter(_.isInstanceOf[Router])
      .foreach{ case router: Router =>
        router.routeTable.foreach {
          case(method, subrouter) => subrouter.routeList.foreach {
            case (subpath, subhandlers) => routeTable(method).registerRoute(routePattern + subpath, middleWare ++ subhandlers)
          }
        }
      }
  }

  def route(httpMethod: HTTPServer.Method, httpRequest: HTTPServer.Request): ApplicationServer.Response = {
    val response = new ApplicationServer.Response(httpRequest.headers)
    routeTable(httpMethod).routeRequest(httpRequest, response)
    return response
  }

  case class SimpleRouter(var routeList: List[(String, List[ApplicationServer.Handler])],
                            var fastRoute: Boolean = true) {
    def registerRoute(routePattern: String, handler: List[ApplicationServer.Handler]): Unit = if (handler.nonEmpty) {
      if(routePattern.contains(":")) {
        fastRoute = false
      }
      routeList ++= List((routePattern, handler))
    }

    def routeRequest(httpRequest: HTTPServer.Request, response: ApplicationServer.Response): Unit =
      routeList
        .map { case (pattern, handlers) => (patternMatches(httpRequest.location, pattern), handlers) }
        .find { case (route, handlers) => route.isDefined }
        .foreach { case (route, handlers) =>
          val req = new ApplicationServer.Request(httpRequest, route.get)
          Router.this.middleWare.foreach(handler => handler.apply(req, response))
          handlers.foreach { routeExecutor => routeExecutor.apply(req, response) }}

    private def startsWithPattern(test: String, pattern: String): Option[Map[String, String]] = {
      val testSegments = test.split("/")
      val patternSegments = pattern.split("/")
      if (testSegments.length < patternSegments.length) {
        return Option.empty
      }

      val params = new mutable.HashMap[String, String]()

      val testIterator = testSegments.iterator
      val patternIterator = patternSegments.iterator

      while (patternIterator.hasNext) {
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
