package io.dallen.scallywag.httpserver

import java.net.{InetSocketAddress, SocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class TCPConsumer {
  def consume(buffer: ArrayBuffer[ByteBuffer], bytesRead: Int): (ByteBuffer, Boolean)
}

class TCPServer(port: Int, consumeFactory: () => TCPConsumer) {

  class ChannelBuffer(val consumer: TCPConsumer) {
    val buffer: ArrayBuffer[ByteBuffer] = new ArrayBuffer[ByteBuffer](1)
    buffer.append(ByteBuffer.allocate(16))
  }

  private val socketChannel: ServerSocketChannel = ServerSocketChannel
    .open
    .configureBlocking(false)
    .asInstanceOf[ServerSocketChannel]

  private val selector: Selector = Selector.open()

  private val reqMap = new mutable.HashMap[SocketAddress, ChannelBuffer]()

  def start(): Unit = {
    socketChannel.bind(new InetSocketAddress(port))
    socketChannel.register(selector, SelectionKey.OP_ACCEPT)

    while (true) {
      checkNewKeys()
    }
  }

  private def checkNewKeys(): Unit = if(selector.selectNow() <= 0) {
    selector
      .selectedKeys()
      .toArray(Array[SelectionKey]())
      .map(acceptOrReadKey)
      .filter(_.isDefined)
      .map(_.get)
      .foreach(handleKey)
    selector.selectedKeys().clear()
  }

  private def handleKey(data: (ChannelBuffer, SocketChannel, Int)): Unit = data match {
    case (channelBuffer, channel, bytesRead) => {
      val (response, close) = channelBuffer.consumer.consume(channelBuffer.buffer, bytesRead)
      writeMessage(channel, response, close)
    }
  }

  private def writeMessage(channel: SocketChannel, msg: ByteBuffer, close: Boolean): Unit = if(channel.isOpen){
    channel.write(msg)
    if (close) {
      channel.close()
    }
  }

  private def acceptOrReadKey(key: SelectionKey): Option[(ChannelBuffer, SocketChannel, Int)] = {
    if (key.isAcceptable) {
      val socketChannel = key.channel.asInstanceOf[ServerSocketChannel].accept
      socketChannel.configureBlocking(false)
      socketChannel.register(key.selector(), SelectionKey.OP_READ)
    }
    if (key.isReadable) {
      val socketChannel = key.channel.asInstanceOf[SocketChannel]
      val writeSpace = reqMap.getOrElseUpdate(socketChannel.getRemoteAddress, new ChannelBuffer(consumeFactory.apply()))
      if(!writeSpace.buffer.last.hasRemaining) {
        writeSpace.buffer.append(ByteBuffer.allocate(16))
      }
      val bytesRead = socketChannel.read(writeSpace.buffer.last)
      return Some((writeSpace, socketChannel, bytesRead))
    }
    return Option.empty
  }
}