package io.dallen.scallywag

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object Main {

  var server: ApplicationServer = null

  def indexGet(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    println(res.headers)
    res.body = "<html><body><h1>Hello, World!</h1></body></html>"
  }

  def otherHello(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    res.body = "<html><body><h1>Hello " + req.routeParameters("name") + "!</h1></body></html>"
  }

  def otherBye(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    res.body = "<html><body><h1>Bye " + req.routeParameters("name") + "!</h1></body></html>"
  }

  def postData(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    println(req.body)
    res.body = s"Got post data with ${req.body.toString}"
  }

  def main(args: Array[String]): Unit = {
    server = new ApplicationServer(8080)
      .get("/index", indexGet _)
      .get("/:name", new Router()
        .get("/hello", otherHello _)
        .get("/bye", otherBye _))
      .post("/", postData _)
    Await.result(server.start(), Duration.Inf)
  }
}

