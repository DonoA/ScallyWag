package io.dallen.scallywag

object Main {

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

  def main(args: Array[String]): Unit = {
    new ApplicationServer(8080)
      .get("/index", indexGet _)
      .get("/:name", new Router()
        .get("/hello", otherHello _)
        .get("/bye", otherBye _))
      .start()
  }
}

