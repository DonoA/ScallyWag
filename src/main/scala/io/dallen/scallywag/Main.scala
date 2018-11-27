package io.dallen.scallywag

object Main {

  def indexGet(req: HTTPApplicationServer.Request, res: HTTPApplicationServer.Response): Unit = {
    println(res.headers)
    res.body = "<html><body><h1>Hello, World!</h1></body></html>"
  }

  def otherGet(req: HTTPApplicationServer.Request, res: HTTPApplicationServer.Response): Unit = {
    res.body = "<html><body><h1>Hello " + req.routeParameters("name") + "!</h1></body></html>"
  }

  def main(args: Array[String]): Unit = {
    new HTTPApplicationServer(8080)
      .get("/index", new HTTPRouter()
        .get("/nice", indexGet _, otherGet _, new HTTPRouter()))
      .get("/:name/hello", otherGet _)
      .start()
  }
}

