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

        ScalaSequel.establishConnection("root", "root", "scallywag", drop = true)

        ScalaSequel.registerSequelType(Job, classOf[Job])
        ScalaSequel.registerSequelType(Person, classOf[Person])
        val tom = new Person("Tom")
        tom.job := new Job("Developer")
        tom.create()
        val stillTom = Person.find(1)
        println(stillTom)
        println(stillTom.job.get)

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