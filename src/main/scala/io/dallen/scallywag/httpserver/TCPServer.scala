package io.dallen.scallywag.httpserver

import java.net.{InetSocketAddress, SocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}

import scala.collection.mutable

class TCPServer(port: Int, handler: (String, SocketChannel) => String) {

  private val socketChannel: ServerSocketChannel = ServerSocketChannel
    .open
    .configureBlocking(false)
    .asInstanceOf[ServerSocketChannel]

  private val selector: Selector = Selector.open()

  private val reqMap = new mutable.HashMap[SocketAddress, mutable.StringBuilder]()

  def start(): Unit = {
    socketChannel.bind(new InetSocketAddress(port))
    socketChannel.register(selector, SelectionKey.OP_ACCEPT)

    while (true) {
      checkNewKeys().foreach(handleKey)
    }
  }

  private def checkNewKeys(): List[(String, SocketChannel, Boolean)] = if(selector.selectNow() <= 0) {
    val keyData = selector
      .selectedKeys()
      .toArray(Array[SelectionKey]())
      .map(acceptOrReadKey)
      .filter(_.isDefined)
      .map(_.get)
    selector.selectedKeys().clear()
    return keyData.toList
  } else {
    List()
  }

  private def handleKey(data: (String, SocketChannel, Boolean)): Unit = data match {
    case (msg, channel, completed) => {
      val existingData = reqMap.getOrElseUpdate(channel.getRemoteAddress, new mutable.StringBuilder())
      if (!msg.isEmpty) {
        existingData.append(msg.replace("\0", ""))
      }
      if (completed) {
        reqMap.remove(channel.getRemoteAddress)
        if (existingData.nonEmpty) {
          val response = handler.apply(existingData.toString(), channel)
          writeMessage(channel, response)
        } else {
          println("Empty request!")
        }
      }
    }
  }

  private def writeMessage(channel: SocketChannel, msg: String): Unit = if(channel.isOpen){
    channel.write(ByteBuffer.wrap(msg.getBytes()))
    channel.close()
  }

  private def acceptOrReadKey(key: SelectionKey): Option[(String, SocketChannel, Boolean)] = {
    if (key.isAcceptable) {
      val socketChannel = key.channel.asInstanceOf[ServerSocketChannel].accept
      socketChannel.configureBlocking(false)
      socketChannel.register(key.selector(), SelectionKey.OP_READ)
    }
    if (key.isReadable) {
      val socketChannel = key.channel.asInstanceOf[SocketChannel]
      val iBuffer = ByteBuffer.allocate(16)
      if (socketChannel.read(iBuffer) <= 0) {
        socketChannel.close()
        return Option.empty
      }

      val msg = new String(iBuffer.array)
      val completed = msg.isEmpty || msg.last == 0
      return Some((msg, socketChannel, completed))
    }
    return Option.empty
  }
}