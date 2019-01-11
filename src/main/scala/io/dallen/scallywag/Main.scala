package io.dallen.scallywag

import io.dallen.scallywag.appserver.{ApplicationServer, Router}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {

  var server: ApplicationServer = _

  def getData(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    println("get data", req.getContext("data"))
  }

  def postData(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    println("post data", req.getContext("data"))
  }

  def enrichData(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    req.addContext("data", "lookup on " + req.routeParameters("id"))
  }

  def main(args: Array[String]): Unit = {
    server = new ApplicationServer(8080)
      .use("/:id/data", enrichData _, new Router()
        .get("/anything", getData _)
        .post("/post_dat", postData _))
    println(server.getRouteMap)
    val f = server.start()
    println("Server started on", server.getPort)
    Await.result(f, Duration.Inf)
  }
}

