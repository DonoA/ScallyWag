package io.dallen.scallywag

import java.io.{BufferedReader, InputStreamReader}
import java.util.regex.Pattern

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.{immutable, mutable}

object ScalaServerApplication {
    
    case class ScalaRequest(location: String, 
                            locationParams: Map[String, String], 
                            requestParams: Map[String, String],
                            headers: ArrayBuffer[ScalaServer.HeaderType],
                            addedData: Map[String, Object])

    case class ScalaResponse(var headers: ArrayBuffer[ScalaServer.HeaderType],
                             var code: (Int, String),
                             writeSocket: String => Unit,
                             renderEngine: ViewRenderer) {
        def renderView(view: String, params: immutable.HashMap[String, Object]) {
            val returnBuilder = new StringBuilder()

            returnBuilder.append(
                ScalaServer.generateHeader(ScalaServer.PreRespHeader("HTTP", 1.0, code._1, code._2)) + "\r\n")
            formatHeaders(returnBuilder, headers)

            if(this.getClass.getResourceAsStream("/views/" + view + ".html.ejs") != null){
                val viewReader = new BufferedReader(new InputStreamReader(
                    this.getClass.getResourceAsStream("/views/" + view + ".html.ejs")))

                returnBuilder.append(renderEngine.parseDocument(viewReader, params).mkString)
            }

            returnBuilder.append("\r\n")

            writeSocket(returnBuilder.mkString)
        }
        
        def sendData(data: String){
            val returnBuilder = new StringBuilder()
            
            returnBuilder.append(
                ScalaServer.generateHeader(ScalaServer.PreRespHeader("HTTP", 1.0, code._1, code._2)) + "\r\n")
            formatHeaders(returnBuilder, headers)
            
            returnBuilder.append(data)
            
            writeSocket(returnBuilder.mkString)
        }
        
        def formatHeaders(strBldr: StringBuilder, headers: ArrayBuffer[ScalaServer.HeaderType]){
            
            for(header <- headers){
                if(header.isInstanceOf[ScalaServer.Header]){
                    strBldr.append(ScalaServer.generateHeader(header.asInstanceOf[ScalaServer.Header]) + "\r\n")
                }
            }

            strBldr.append("\r\n")
        }
    }
    
    trait RequestMethod

    object RequestMethod {
        case object GET extends RequestMethod
        case object POST extends RequestMethod
        case object PUT extends RequestMethod
        case object DELETE extends RequestMethod
    }
    
    val routeSelectorPatern: Pattern = Pattern.compile("\\/\\:([^\\/]+)")
    val routeReplacePatern: Pattern = Pattern.compile("\\:[^\\/]+")

    type RequestHandler = (ScalaServerApplication.ScalaRequest, 
                           ScalaServerApplication.ScalaResponse, 
                           (Seq[(String, Object)]) => Unit) => Unit
    
    abstract case class Route(pattern: RoutePattern, 
                              method: ScalaServerApplication.RequestMethod,
                              handlers: Seq[RequestHandler])

    class GetRoute(pattern: RoutePattern, 
                   handlers: Seq[RequestHandler]
    ) extends Route(pattern, ScalaServerApplication.RequestMethod.GET, handlers)

    class PostRoute(pattern: RoutePattern, 
                    handlers: Seq[RequestHandler]
    ) extends Route(pattern, ScalaServerApplication.RequestMethod.POST, handlers)

    class PutRoute(pattern: RoutePattern, 
                   handlers: Seq[RequestHandler]
    ) extends Route(pattern, ScalaServerApplication.RequestMethod.PUT, handlers)

    class DeleteRoute(pattern: RoutePattern, 
                      handlers: Seq[RequestHandler]
    ) extends Route(pattern, ScalaServerApplication.RequestMethod.DELETE, handlers)



    class RoutePattern(rawPath: String) {

        val regexPatern: Pattern = generateRegex(rawPath)
        val params: Array[String] = locateParams(rawPath)

        def locateParams(path: String): Array[String] = {
            val matcher = ScalaServerApplication.routeSelectorPatern.matcher(path)
            var prms = ArrayBuffer[String]()
            while(matcher.find){
                prms += matcher.group(1)
            }
            return prms.toArray
        }

        def generateRegex(path: String): Pattern = {
            val matcher = ScalaServerApplication.routeReplacePatern.matcher(path.replace("/", "\\/"))
            val pat = matcher.replaceAll("([^\\/]+)")
            return Pattern.compile("^" + pat + "\\/?$")
        }
    }
}

class ScalaServerApplication {
    
    var server: ScalaServer = _

    var viewRenderer: ViewRenderer = _
    
    def bind(port: Int) {
        server = new ScalaServer(port, handleRequest)
        server.start
    }

    def setViewRenderEngine(engine: ViewRenderer): Unit = viewRenderer = engine
    
    def handleRequest(headers: ArrayBuffer[ScalaServer.HeaderType], writeSocket: String => Unit) {
        val location = headers(0).asInstanceOf[ScalaServer.PreReqHeader].path
        val method = headers(0).asInstanceOf[ScalaServer.PreReqHeader].method match {
            case "GET" => ScalaServerApplication.RequestMethod.GET
            case "POST" => ScalaServerApplication.RequestMethod.POST
            case "PUT" => ScalaServerApplication.RequestMethod.PUT
            case "DELETE" => ScalaServerApplication.RequestMethod.DELETE
        }
        
        val params = new mutable.HashMap[String, String]()
        
        val route = Router.getRoute(method, location, params)
        
        val req = ScalaServerApplication.ScalaRequest(location,
                                                          params.toMap, 
                                                          Map[String, String](),
                                                          headers,
                                                          Map[String, Object]())
        val resp = ScalaServerApplication.ScalaResponse(
            ScalaServer.Header("Content-Type", "text/html") +: new ArrayBuffer[ScalaServer.HeaderType](),
            (200, "OK"),
            writeSocket,
            viewRenderer
        )
        def next(tups: (String, Object)*) {
            println("next call")
        }
        for(handler <- route.handlers){
            handler(req, resp, next)
        }
    }
    
    object Router {

        var routes: List[ScalaServerApplication.Route] = List[ScalaServerApplication.Route]()

        var notFoundRoute: ScalaServerApplication.GetRoute = _
        
        def createRoute(method: ScalaServerApplication.RequestMethod,
                        pattern: String, 
                        handlers: ScalaServerApplication.RequestHandler*) {
            val routePattern = new ScalaServerApplication.RoutePattern(pattern)
            method match {
                case _:ScalaServerApplication.RequestMethod.GET.type => 
                    routes = new ScalaServerApplication.GetRoute(routePattern, handlers) :: routes
                case _:ScalaServerApplication.RequestMethod.POST.type => 
                    routes = new ScalaServerApplication.PostRoute(routePattern, handlers) :: routes
                case _:ScalaServerApplication.RequestMethod.PUT.type => 
                    routes = new ScalaServerApplication.PutRoute(routePattern, handlers) :: routes
                case _:ScalaServerApplication.RequestMethod.DELETE.type => 
                    routes = new ScalaServerApplication.DeleteRoute(routePattern, handlers) :: routes
            }
        }

        def setNotFoundRoute(handlers: ScalaServerApplication.RequestHandler*){
            notFoundRoute = new ScalaServerApplication.GetRoute(
                new ScalaServerApplication.RoutePattern("/notfound"), handlers
            )
        }
        
        def setPublicPrefix(prefix: String) {
            def handlePublic(req: ScalaServerApplication.ScalaRequest, 
                             resp: ScalaServerApplication.ScalaResponse, 
                             next: ((String, Object)*) => Unit) {
                
                val dataBuilder = new StringBuilder()
                
                val dataReader = new BufferedReader(new InputStreamReader(
                    this.getClass.getResourceAsStream("/public/" + req.locationParams("location"))))
                var line: String = dataReader.readLine
                while(line != null){
                    dataBuilder.append(line + "\r\n")
                    line = dataReader.readLine
                }
                
                resp.headers(0).asInstanceOf[ScalaServer.Header].value = "text"
                
                resp.sendData(dataBuilder.mkString)
                
            }
            createRoute(ScalaServerApplication.RequestMethod.GET, prefix + "/:location", handlePublic)
        }
        
        def getRoute(method: ScalaServerApplication.RequestMethod, 
                     location: String,
                     params: mutable.HashMap[String, String]): ScalaServerApplication.Route = {
            for(route <- routes){
                if(route.method == method && evalPatern(route.pattern, location, params)){
                    return route
                }
            }
            return notFoundRoute
        }
        
        def evalPatern(pattern: ScalaServerApplication.RoutePattern, 
                       location: String, 
                       params: mutable.HashMap[String, String]): Boolean = {
            val matcher = pattern.regexPatern.matcher(location)
            if(matcher.matches){
                val res = matcher.toMatchResult
                for(i <- 0 to res.groupCount - 1) {
                    params += (pattern.params(i) -> res.group(i+1))
                }
                return true
            } else {
                return false
            }
        }
    }
}