package io.dallen.scallywag

object Main {

  def indexGet(req: HTTPApplicationServer.Request, res: HTTPApplicationServer.Response): Unit = {
    res.body = "<html><body><h1>Hello, World!</h1></body></html>"
  }

  def main(args: Array[String]): Unit = {
    new HTTPApplicationServer(8080)
      .get("/", indexGet)
      .start()
  }
}

