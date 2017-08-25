package gov.noaa.scalamaven

import java.io.BufferedReader
import java.io.InputStreamReader
import scala.collection.mutable.HashMap

object ScalaMaven {
    
    def handleGet(req: ScalaServerApplication.ScalaRequest, 
                  resp: ScalaServerApplication.ScalaResponse, 
                  next: ((String, Object)*) => Unit) {
        resp.renderView("index")
    }
    
    def handleNotFoundGet(req: ScalaServerApplication.ScalaRequest, 
                          resp: ScalaServerApplication.ScalaResponse, 
                          next: ((String, Object)*) => Unit) {
        resp.code = (404, "Not Found")
        resp.renderView("404")
    }

    def main(args: Array[String]) {
        
        val dataReader = new BufferedReader(new InputStreamReader(
                    this.getClass.getResourceAsStream("/views/sample.html.ejs")))
            
        val scopeData = new HashMap[String, Object]()
        
        scopeData.put("title", "Sample title")
        scopeData.put("ttl", "dat")
        
        val nDoc = ScalaViewer.parseDocument(dataReader, scopeData)
        
        println(nDoc.mkString)
        
//        val app = new ScalaServerApplication()
//        app.bind(8998)
//        
//        app.Router.createRoute(ScalaServerApplication.RequestMethod.GET, "/", handleGet)
//        app.Router.createRoute(ScalaServerApplication.RequestMethod.GET, "/test/:person/home/:task", handleGet)
//        app.Router.setNotFoundRoute(handleNotFoundGet)
//        app.Router.setPublicPrefix("/public")
    }
    
}