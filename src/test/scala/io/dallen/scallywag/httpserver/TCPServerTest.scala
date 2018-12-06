package io.dallen.scallywag.httpserver
import java.net.{InetSocketAddress, ServerSocket, SocketAddress, SocketOption}
import java.nio.ByteBuffer
import java.nio.channels._
import java.util

import org.mockito.Matchers._
import org.mockito.Mockito
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.scalatest._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


class TCPServerTest extends FlatSpec {

  it should "bind to address and port" in {

    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])

    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[(ByteBuffer, Boolean)] = {
      return Option.empty
    }

    val portToBind = 3000

    val server = new TCPServer(portToBind, () => handler, mockServerSocketChannel, mockSelector,
      TCPServer.simpleClientSocketChannelFactory, TCPServer.simpleClientSelectionKeyFactory)
    server.start()

    verify(mockServerSocketChannel, times(1)).open(new InetSocketAddress(portToBind), mockSelector)

    server.stop()

    verify(mockServerSocketChannel, times(1)).close()
  }

  it should "accept new keys" in {
    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])
    val mockAcceptable = Mockito.mock(classOf[TCPServer.ClientSelectionKeyImpl])
    val mockClientChannel = Mockito.mock(classOf[TCPServer.ClientSocketChannelImpl])
    val placeholderMockAccpetable = Mockito.mock(classOf[SelectionKey])

    val toReturn = new util.HashSet[SelectionKey]()
    var acceptable = true
    val channel = new ServerSocketChannel(null) {
      override def bind(socketAddress: SocketAddress, i: Int): ServerSocketChannel = ???
      override def setOption[T](socketOption: SocketOption[T], t: T): ServerSocketChannel = ???
      override def socket(): ServerSocket = ???
      override def accept(): SocketChannel = null
      override def getLocalAddress: SocketAddress = ???
      override def getOption[T](socketOption: SocketOption[T]): T = ???
      override def supportedOptions(): util.Set[SocketOption[_]] = ???
      override def implCloseSelectableChannel(): Unit = ???
      override def implConfigureBlocking(b: Boolean): Unit = ???
    }

    when(mockSelector.selectNow()).thenAnswer((_: InvocationOnMock) => toReturn.size)
    when(mockSelector.selectedKeys()).thenAnswer((_: InvocationOnMock) => toReturn)
    when(mockAcceptable.isAcceptable).thenAnswer((_: InvocationOnMock) => acceptable)
    when(mockAcceptable.channel).thenAnswer((_: InvocationOnMock) => channel)
    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[(ByteBuffer, Boolean)] = {
      return Option.empty
    }

    val server = new TCPServer(3000, () => handler, mockServerSocketChannel, mockSelector, _ => mockClientChannel,
      _ => mockAcceptable)

    when(mockClientChannel.accept(any(), any())).thenAnswer((_: InvocationOnMock) => {
      acceptable = false
      server.stop()
      println("Stop ", System.currentTimeMillis())
      null
    })

    toReturn.add(placeholderMockAccpetable)

    implicit val ec: ExecutionContext = ExecutionContext.global
    val f = Future {
      println("Start", System.currentTimeMillis())
      server.start()
      server.await()
      true
    }

    println(Await.ready(f, Duration(100, "millis")))
  }

  it should "read data from existing keys" in {

  }

  it should "not block between read cycles" in {

  }

  it should "map data to channel handlers" in {

  }

  it should "write data to channels" in {

  }
}
