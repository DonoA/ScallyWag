package io.dallen.scallywag

import io.dallen.scallywag.appserver.{ApplicationServer, Router}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {

  var server: ApplicationServer = _

  def postData(req: ApplicationServer.Request, res: ApplicationServer.Response): Unit = {
    println(req.headers)
    res.body = s"Got post data with ${req.body.toString} and ${req.cookie("lijitcookie")}"
  }

  def main(args: Array[String]): Unit = {
    server = new ApplicationServer(8080)
      .post("/", postData _)
    println(server.getRouteMap)
    val f = server.start()
    println("Server started on", server.getPort)
    Await.result(f, Duration.Inf)
  }
}

