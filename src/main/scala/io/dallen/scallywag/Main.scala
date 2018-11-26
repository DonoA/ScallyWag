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
      .get("/", indexGet)
      .get("/:name/hello", otherGet)
      .start()
  }
}

