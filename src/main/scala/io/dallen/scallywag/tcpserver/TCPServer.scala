package io.dallen.scallywag.tcpserver

import java.net.{InetSocketAddress, Socket, SocketAddress}
import java.nio.ByteBuffer
import java.nio.channels._
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

object TCPServer {
  type TCPConsumer = (ArrayBuffer[ByteBuffer], Int) => Option[(ByteBuffer, Boolean)]

  val simpleClientSocketChannelFactory: SocketChannel => ClientSocketChannel =
    sc => new TCPServer.ClientSocketChannelImpl(sc)

  val simpleClientSelectionKeyFactory: SelectionKey => ClientSelectionKey =
    sk => new TCPServer.ClientSelectionKeyImpl(sk)

  class AcceptingSocketChannelImpl(serverSocketChannel: ServerSocketChannel) extends AcceptingSocketChannel {
    override def open(inetSocketAddress: InetSocketAddress, selector: Selector): Unit = {
      serverSocketChannel.bind(inetSocketAddress)
      serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT)
    }

    override def close(): Unit = serverSocketChannel.close()
  }

  class ClientSocketChannelImpl(channel: SocketChannel) extends ClientSocketChannel {
    override def accept(block: Boolean, selector: Selector): ClientSocketChannel = {
      channel.configureBlocking(block)
      channel.register(selector, SelectionKey.OP_READ)
      return this
    }

    override def getRemoteAddress: SocketAddress = channel.getRemoteAddress
    override def isOpen: Boolean = channel.isOpen
    override def read(buffer: ByteBuffer): Int = channel.read(buffer)
    override def write(buffer: ByteBuffer): Int = channel.write(buffer)
    override def close(): Unit = channel.close()
    override def socket: Socket = channel.socket()
  }

  class ClientSelectionKeyImpl(key: SelectionKey) extends ClientSelectionKey {
    override def isAcceptable: Boolean = key.isAcceptable
    override def isReadable: Boolean = key.isReadable
    override def channel: SelectableChannel = key.channel()
    override def selector: Selector = key.selector()
  }

  class ChannelBuffer(val consumer: TCPServer.TCPConsumer, val socket: Socket) {
    val buffer: ArrayBuffer[ByteBuffer] = new ArrayBuffer[ByteBuffer](1)
    buffer.append(ByteBuffer.allocate(16))
  }
}

class TCPServer(port: Int, consumeFactory: () => TCPServer.TCPConsumer, socketChannel: AcceptingSocketChannel,
                selector: Selector, clientSocketChannelFactory: SocketChannel => ClientSocketChannel,
                clientSelectionKeyFactory: SelectionKey => ClientSelectionKey) {

  private val running: AtomicBoolean = new AtomicBoolean(false)

  private var reqMap = new mutable.HashMap[SocketAddress, TCPServer.ChannelBuffer]()

  def this(port: Int, consumeFactory: () => TCPServer.TCPConsumer) {
    this(port,
      consumeFactory,
      new TCPServer.AcceptingSocketChannelImpl(ServerSocketChannel
        .open
        .configureBlocking(false)
        .asInstanceOf[ServerSocketChannel]),
      Selector.open(),
      TCPServer.simpleClientSocketChannelFactory,
      TCPServer.simpleClientSelectionKeyFactory)
  }

  def start(): Future[Unit] = {
    running.set(true)
    socketChannel.open(new InetSocketAddress(port), selector)
    return Future({
      run()
    })(ExecutionContext.global)
  }

  def stop(): Unit = {
    running.set(false)
    socketChannel.close()
  }

  private def run(): Unit = {
    while (running.get()) {
      checkNewKeys()
      cleanOldKeys()
    }
  }

  private def checkNewKeys(): Unit = if(selector.selectNow() > 0) {
    selector
      .selectedKeys()
      .toArray(Array[SelectionKey]())
      .map(sk => clientSelectionKeyFactory.apply(sk))
      .map(acceptOrReadKey)
      .filter(_.isDefined)
      .map(_.get)
      .foreach(handleKey)
    selector.selectedKeys().clear()
  }

  private def cleanOldKeys(): Unit = {
    reqMap = reqMap.filter {
      case (addr, buffer) => !buffer.socket.isClosed
    }
  }

  private def handleKey(data: (TCPServer.ChannelBuffer, ClientSocketChannel, Int)): Unit = data match {
    case (channelBuffer, channel, bytesRead) => {
      val channelAction = channelBuffer.consumer.apply(channelBuffer.buffer, bytesRead)
      channelAction.foreach {
        case (response, close) => writeMessage(channel, response, close)
      }
    }
  }

  private def writeMessage(channel: ClientSocketChannel, msg: ByteBuffer, close: Boolean): Unit = if(channel.isOpen){
    channel.write(msg)
    if (close) {
      channel.close()
    }
  }

  private def acceptOrReadKey(key: ClientSelectionKey): Option[(TCPServer.ChannelBuffer, ClientSocketChannel, Int)] = {
    if (key.isAcceptable) {
      val socketChannel =
        clientSocketChannelFactory.apply(key.channel.asInstanceOf[ServerSocketChannel].accept())
          .accept(false, key.selector)
    }
    if (key.isReadable) {
      val socketChannel =
        clientSocketChannelFactory.apply(key.channel.asInstanceOf[SocketChannel])
      val writeSpace = reqMap.getOrElseUpdate(socketChannel.getRemoteAddress, new TCPServer.ChannelBuffer(
        consumeFactory.apply(), socketChannel.socket))
      if(writeSpace.buffer.isEmpty || !writeSpace.buffer.last.hasRemaining) {
        writeSpace.buffer.append(ByteBuffer.allocate(16))
      }
      val bytesRead = socketChannel.read(writeSpace.buffer.last)
      return Some((writeSpace, socketChannel, bytesRead))
    }
    return Option.empty
  }
}