package io.dallen.scallywag

import scala.collection.{immutable, mutable}

object SampleApp {

    val someThing = "err"


    def handleGet(req: ScalaServerApplication.ScalaRequest, 
                  resp: ScalaServerApplication.ScalaResponse, 
                  next: ((String, Object)*) => Unit) {
        resp.renderView("sample", immutable.HashMap[String, Object](
                    "title" -> ("Sample title: " + req.locationParams("person")),
                    "person" -> req.locationParams("person"),
                    "task" -> req.locationParams("task")
                ))
    }
    
    def handleNotFoundGet(req: ScalaServerApplication.ScalaRequest, 
                          resp: ScalaServerApplication.ScalaResponse, 
                          next: ((String, Object)*) => Unit) {
        resp.code = (404, "Not Found")
        resp.renderView("404", immutable.HashMap[String, Object]())
    }

    object Job extends ScalaSequel.SequelType[Job]("Job") {

    }

    object Person extends ScalaSequel.SequelType[Person]("Person") {

    }

    case class Person(@SequelField var name: String) extends ScalaSequel.SequelData {
        @SequelField
        val job = Relations.HasOne[Job](this)
    }

    case class Job(@SequelField var title: String) extends ScalaSequel.SequelData {
        @SequelField
        val worker = Relations.BelongsTo[Person](this)
    }

    def main(args: Array[String]) {

        ScalaSequel.establishConnection("localhost", "root", "root", "scallywag", 3306)

        ScalaSequel.registerSequelType(Job, classOf[Job])
        ScalaSequel.registerSequelType(Person, classOf[Person])
        val tom = new Person("Tom")
        val tomsJob = new Job("Developer")
        tom.job := tomsJob
        println(tomsJob.worker.get())
//        tom.create()
//        tomsJob.create()
//        tom.job := tomsJob
//        tom.update()
//        val theSame = BasicData.find(1)
//
//        println(theSame)
        
//        val app = new ScalaServerApplication()
//        app.bind(8998)
//        app.setViewRenderEngine(ScalaEjsViewer)
//
//        app.Router.createRoute(ScalaServerApplication.RequestMethod.GET, "/", handleGet)
//        app.Router.createRoute(ScalaServerApplication.RequestMethod.GET, "/test/:person/home/:task", handleGet)
//        app.Router.setNotFoundRoute(handleNotFoundGet)
//        app.Router.setPublicPrefix("/public")
    }
    
}