package io.dallen.scallywag

import java.lang.{Thread, Double}
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel, SelectionKey, Selector}
import scala.collection.mutable.{ArrayBuffer, Queue}
import collection.JavaConverters._

object ScalaServer {

    type RequestHanderFunction = ((ArrayBuffer[ScalaServer.HeaderType], String => Unit) => Unit)

    abstract class HeaderType
    
    case class PreReqHeader(method: String, path: String, proto: String, number: Double
        ) extends HeaderType

    case class PreRespHeader(proto: String, number: Double, code: Int, name: String) extends HeaderType

    case class Header(name: String, var value: String) extends HeaderType
    
    def generateHeader(header: ScalaServer.HeaderType): String = header match {
        case ScalaServer.PreRespHeader(proto, number, code, name) => proto + "/" + number + " " + code + " " + name
        case ScalaServer.Header(name, value) => name + ": " + value
    } 

    def parseHeader(str: String): ScalaServer.HeaderType = {
        if(!str.contains(": ")){
            if(str.contains(" ") && str.contains("/")){
                val peiceArr = str.split(" ")
                val protoTyp = peiceArr(2).split("/")
                val method = peiceArr(0)
                return ScalaServer.PreReqHeader(method, peiceArr(1), protoTyp(0), Double.parseDouble(protoTyp(1)))
            }else{
                return null
            }
        }
        val bits = str.split(": ")
        return ScalaServer.Header(bits(0), bits(1))
    }
    
    case class RequestForParse(readData: String, channel: SocketChannel)
    
    class HTTPHanlerThread(handlers: Seq[(ArrayBuffer[ScalaServer.HeaderType], String => Unit) => Unit]
            ) extends Thread {
        
        var running = true

        var busy = false

        @volatile var toHandle = new Queue[RequestForParse]()

        def enqueueRequest(data: RequestForParse) {
            toHandle += data
        }

        def isBusy: Boolean = busy

        override def run() {
            while(running){
                if(toHandle.isEmpty){
                    Thread.sleep(10)
                }
                while(!toHandle.isEmpty){
                    busy = true
                    val req = toHandle.dequeue
                    println("Connection from " + 
                            req.channel.getRemoteAddress.asInstanceOf[InetSocketAddress].getHostString + " port " +
                            req.channel.getRemoteAddress.asInstanceOf[InetSocketAddress].getPort)
                    handleReq(req)
                    busy = false
                }
            }
        }
        
        def handleReq(req: RequestForParse) {
            val lines = req.readData.split("[\n\r]")
            val sockChannel = req.channel
            sockChannel.configureBlocking(false)
            val headers = new ArrayBuffer[ScalaServer.HeaderType]()
            for(line <- lines){
                val head = parseHeader(line)
                if(head != null) {
                    headers += head
                }
            }
            
            def writter(toWrite: String) {
                if(sockChannel.isOpen){
                    sockChannel.write(ByteBuffer.wrap(toWrite.getBytes()))
                    sockChannel.close
                }else{
                }
            }
            
            for(handler <- handlers){
                handler(headers, writter)
            }
        }
    }
}

class ScalaServer(port: Int, onRequest: ScalaServer.RequestHanderFunction*) extends Thread {
    
    var running = true

    def isRunning: Boolean = running

    var sSock: ServerSocketChannel = _
    var handlers: List[ScalaServer.HTTPHanlerThread] = List[ScalaServer.HTTPHanlerThread]()
    val selector: Selector = Selector.open()

    override def run(){
        sSock = ServerSocketChannel.open
                                   .configureBlocking(false)
                                   .asInstanceOf[ServerSocketChannel]
                                   .bind(new InetSocketAddress(port))
        sSock.register(selector, SelectionKey.OP_ACCEPT)
        1 to 4 foreach {
            i => {
                val handle = new ScalaServer.HTTPHanlerThread(onRequest)
                handle.start()
                handle.setName("ReqHandler-" + i)
                handlers = handle :: handlers
            }
        }
        println("Spinning up " + handlers.length + " worker threads and 1 server thread")
        while(true) {
            if(selector.selectNow > 0) {
                val ittr = selector.selectedKeys().iterator
                while(ittr.hasNext){
                    val key = ittr.next
                    ittr.remove
                    val req = acceptOrReadKey(key)
                    if(req != null){
                        var i = 0
                        var thread = handlers(i)
                        while(thread.isBusy && handlers.length-1 > i){
                            i += 1
                            thread = handlers(i)
                        }
                        handlers(i).enqueueRequest(req)
                    }
                }
            }
        }
        sSock.close()
    }
    
    def acceptOrReadKey(key: SelectionKey): ScalaServer.RequestForParse = {
        if (key.isAcceptable) {
            val sockChannel = key.channel.asInstanceOf[ServerSocketChannel]
                    .accept.asInstanceOf[SocketChannel]
            sockChannel.configureBlocking(false)
            sockChannel.register(key.selector(), SelectionKey.OP_READ)
        }
        if (key.isReadable) {
            val sockChannel = key.channel.asInstanceOf[SocketChannel]
            val iBuffer = ByteBuffer.allocate(1024)
            val bytesCount = sockChannel.read(iBuffer)
            if(bytesCount <= 0){
                sockChannel.close
                return null
            }
            val inputData = new String(iBuffer.array)
            return ScalaServer.RequestForParse(inputData, sockChannel)
        }
        return null
    }
}
