package gov.noaa.scalamaven

import java.io.{BufferedReader, InputStreamReader}
import java.util.regex.Pattern
import scala.collection.mutable.{ArrayBuffer, HashMap}

object ScalaServerApplication {
    
    case class ScalaRequest(location: String, 
                            locationParams: Map[String, String], 
                            requestParams: Map[String, String],
                            headers: ArrayBuffer[ScalaServer.HeaderType],
                            addedData: Map[String, Object])

    case class ScalaResponse(var headers: ArrayBuffer[ScalaServer.HeaderType],
                             var code: (Int, String),
                             writeSocket: String => Unit) {
        def renderView(view: String) {
            val returnBuilder = new StringBuilder()
            
            returnBuilder.append(
                ScalaServer.generateHeader(ScalaServer.PreRespHeader("HTTP", 1.0, code._1, code._2)) + "\r\n")
            formatHeaders(returnBuilder, headers)
            
            println(view)
            
            val viewReader = new BufferedReader(new InputStreamReader(
                    this.getClass.getResourceAsStream("/views/" + view + ".html")))
            var line: String = viewReader.readLine;
            while(line != null){
                returnBuilder.append(line + "\r\n")
                line = viewReader.readLine
            }

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
    
    val routeSelectorPatern = Pattern.compile("\\/\\:([^\\/]+)")
    val routeReplacePatern = Pattern.compile("\\:[^\\/]+")

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
    
    var server: ScalaServer = null
    
    def bind(port: Int) {
        server = new ScalaServer(port, handleRequest)
        server.start
    }
    
    def handleRequest(headers: ArrayBuffer[ScalaServer.HeaderType], writeSocket: String => Unit) {
        val location = headers(0).asInstanceOf[ScalaServer.PreReqHeader].path
        val method = headers(0).asInstanceOf[ScalaServer.PreReqHeader].method match {
            case "GET" => ScalaServerApplication.RequestMethod.GET
            case "POST" => ScalaServerApplication.RequestMethod.POST
            case "PUT" => ScalaServerApplication.RequestMethod.PUT
            case "DELETE" => ScalaServerApplication.RequestMethod.DELETE
        }
        
        val params = new HashMap[String, String]()
        
        val route = Router.getRoute(method, location, params)
        
        val req = new ScalaServerApplication.ScalaRequest(location, 
                                                          params.toMap, 
                                                          Map[String, String](),
                                                          headers,
                                                          Map[String, Object]())
        val resp = new ScalaServerApplication.ScalaResponse(
            ScalaServer.Header("Content-Type", "text/html") +: new ArrayBuffer[ScalaServer.HeaderType](),
            (200, "OK"),
            writeSocket
        )
        def next(tups: (String, Object)*) {
            println("next call")
        }
        for(handler <- route.handlers){
            handler(req, resp, next)
        }
    }
    
    object Router {

        var routes = List[ScalaServerApplication.Route]()

        var notFoundRoute: ScalaServerApplication.GetRoute = null
        
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
                var line: String = dataReader.readLine;
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
                     params: HashMap[String, String]): ScalaServerApplication.Route = {
            for(route <- routes){
                if(route.method == method && evalPatern(route.pattern, location, params)){
                    println(params.toString)
                    return route
                }
            }
            return notFoundRoute
        }
        
        def evalPatern(pattern: ScalaServerApplication.RoutePattern, 
                       location: String, 
                       params: HashMap[String, String]): Boolean = {
            val matcher = pattern.regexPatern.matcher(location)
            if(matcher.matches){
                var res = matcher.toMatchResult
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