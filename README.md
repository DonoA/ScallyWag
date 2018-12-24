ScallyWag
===
A simple web framework written in Scala

# TCPServer
The TCP Server accepts data from TCP sockets without blocking. A thread checks for updates on all open channels and creates a stream of data segments that can be consumed. The server binds to an AcceptingSocketChannel interface and provides a nio.channels.Selector which should be notified when new data is ready for reading. 

When a new client connects to the TCP server, a new ClientSocketChannel is built to handle data from that channel. In this way, data can be read out over a large number of cycles into an object that maintains state over the whole stream. When data is received on any channel, the respective ClientSocketChannel is given the opportunity to process the data and respond with data of its own.

# HTTPServer
The HTTP Server reads from a stream of data buffers provided by the TCP Server and converts them into a full HTTP request to be enriched and processed. The core of the HTTP Server's logic is in the TCPStreamCollector which implements a simple state machine. In the header reading state, the collector awaits `\r\n\r\n` at which point is parses the header of the request and moves to the body collecting state. The value of `content-length` is used to determine when a complete request has be received. Once a body is collected, `connection: keep-alive` is used to determine if state should be reset and the connection kept open.

Once a full request is requested, the request is passed up the chain and an HTTP response object is awaited.

# ApplicationServer
The application server implements the high level logic of routing requests and executing controllers. 

# ViewBuilder
The view builder renders html based off hierarchies of simple objects. Ideally, these views should be able to integrate existing react.js modules for extensive use.