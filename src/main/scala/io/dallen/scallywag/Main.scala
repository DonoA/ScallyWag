package io.dallen.scallywag

import io.dallen.scallywag.appserver.{ApplicationServer, Router}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {

  var server: ApplicationServer = _

  def postData(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    println("post data")
  }

  def enrichData(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    println("Enrich")
  }

  def main(args: Array[String]): Unit = {
    server = new ApplicationServer(8080)
      .use("/data", enrichData _, new Router()
        .get("/anything", postData _))
    println(server.getRouteMap)
    val f = server.start()
    println("Server started on", server.getPort)
    Await.result(f, Duration.Inf)
  }
}

