package io.dallen.scallywag.tcpserver

import java.io.{BufferedReader, DataOutputStream, InputStreamReader}
import java.net._
import java.nio.ByteBuffer
import java.nio.channels._
import java.util

import org.mockito.Matchers._
import org.mockito.Mockito
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.scalatest._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.Duration


class TCPServerTest extends FlatSpec {

  it should "bind to address and port" in {

    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])

    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[TCPServer.TCPResponse] = None

    val portToBind = 3000

    val server = new TCPServer(portToBind, () => handler, mockServerSocketChannel, mockSelector,
      TCPServer.simpleClientSocketChannelFactory, TCPServer.simpleClientSelectionKeyFactory)

    when(mockSelector.selectNow()).thenAnswer((_: InvocationOnMock) => {
      server.stop()
      0
    })

    Await.result(server.start(), Duration(100, "millis"))

    verify(mockServerSocketChannel, times(1)).open(new InetSocketAddress(portToBind), mockSelector)
    verify(mockServerSocketChannel, times(1)).close()
  }

  it should "accept new keys" in {
    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])
    val mockAcceptable = Mockito.mock(classOf[TCPServer.ClientSelectionKeyImpl])
    val mockClientChannel = Mockito.mock(classOf[TCPServer.ClientSocketChannelImpl])
    val placeholderMockAcceptable = Mockito.mock(classOf[SelectionKey])

    val toReturn = new util.HashSet[SelectionKey]()
    var acceptable = true
    val channel = Mockito.mock(classOf[ServerSocketChannel])

    when(mockSelector.selectNow()).thenAnswer((_: InvocationOnMock) => toReturn.size)
    when(mockSelector.selectedKeys()).thenAnswer((_: InvocationOnMock) => toReturn)
    when(mockAcceptable.isAcceptable).thenAnswer((_: InvocationOnMock) => acceptable)
    when(mockAcceptable.channel).thenAnswer((_: InvocationOnMock) => channel)
    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[TCPServer.TCPResponse] = None

    val server = new TCPServer(3000, () => handler, mockServerSocketChannel, mockSelector, _ => mockClientChannel,
      _ => mockAcceptable)

    when(mockClientChannel.accept(any(), any())).thenAnswer((_: InvocationOnMock) => {
      acceptable = false
      server.stop()
      null
    })

    toReturn.add(placeholderMockAcceptable)

    Await.result(server.start(), Duration(100, "millis"))
  }

  it should "read data from existing keys" in {
    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])
    val mockReadable = Mockito.mock(classOf[TCPServer.ClientSelectionKeyImpl])
    val mockSocket = Mockito.mock(classOf[Socket])
    val mockClientChannel = Mockito.mock(classOf[TCPServer.ClientSocketChannelImpl])
    val placeholderMockReadable = Mockito.mock(classOf[SelectionKey])

    val toReturn = new util.HashSet[SelectionKey]()
    val mockAddress = new InetSocketAddress(12345)
    val data = "Hello"

    when(mockSelector.selectNow()).thenAnswer((_: InvocationOnMock) => toReturn.size)
    when(mockSelector.selectedKeys()).thenAnswer((_: InvocationOnMock) => toReturn)
    when(mockReadable.isAcceptable).thenAnswer((_: InvocationOnMock) => false)
    when(mockReadable.isReadable).thenAnswer((_: InvocationOnMock) => true)
    when(mockSocket.isClosed).thenAnswer((_: InvocationOnMock) => false)
    when(mockClientChannel.socket).thenAnswer((_: InvocationOnMock) => mockSocket)
    when(mockClientChannel.getRemoteAddress).thenAnswer((_: InvocationOnMock) => mockAddress)
    when(mockClientChannel.read(any(classOf[ByteBuffer]))).thenAnswer((params: InvocationOnMock) => {
      val buffer = params.getArgumentAt(0, classOf[ByteBuffer])
      val rawDat = data.getBytes("utf-8")
      buffer.put(rawDat)
      data.length
    })

    var server: TCPServer = null
    var readData: String = null
    var handlerBytesRead: Int = -1

    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[(ByteBuffer, Boolean)] = {
      readData = new String(buffers.last.array()).replace("\0", "")
      handlerBytesRead = bytesRead
      server.stop()
      return None
    }

    val handleProducer = Mockito.mock(classOf[() => TCPServer.TCPConsumer])
    when(handleProducer.apply()).thenAnswer((_: InvocationOnMock) => handler _)

    server = new TCPServer(3000, handleProducer, mockServerSocketChannel, mockSelector, _ => mockClientChannel,
      _ => mockReadable)

    toReturn.add(placeholderMockReadable)

    Await.result(server.start(), Duration(100, "millis"))
    verify(handleProducer, times(1)).apply()
    assert(readData equals data)
    assert(handlerBytesRead equals data.length)
  }

  it should "map data to channel handlers" in {
    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])
    val mockReadable = Mockito.mock(classOf[TCPServer.ClientSelectionKeyImpl])
    val mockClientChannel = Mockito.mock(classOf[TCPServer.ClientSocketChannelImpl])
    val mockSocket = Mockito.mock(classOf[Socket])
    val placeholderMockReadable = Mockito.mock(classOf[SelectionKey])

    val toReturn = new util.HashSet[SelectionKey]()
    val mockAddress1 = new InetSocketAddress(12345)
    val mockAddress2 = new InetSocketAddress(54321)
    val messages = List((mockAddress1, "Hello"), (mockAddress2, "JUNK"), (mockAddress1, "World")).iterator

    var (currentAddress, currentMessage) = messages.next()

    val savedBuffers = new ArrayBuffer[(InetSocketAddress, ArrayBuffer[ByteBuffer])]()

    when(mockSelector.selectNow()).thenAnswer((_: InvocationOnMock) => toReturn.size)
    when(mockSelector.selectedKeys()).thenAnswer((_: InvocationOnMock) => toReturn.clone())
    when(mockReadable.isAcceptable).thenAnswer((_: InvocationOnMock) => false)
    when(mockReadable.isReadable).thenAnswer((_: InvocationOnMock) => true)
    when(mockSocket.isClosed).thenAnswer((_: InvocationOnMock) => false)
    when(mockClientChannel.getRemoteAddress).thenAnswer((_: InvocationOnMock) => currentAddress)
    when(mockClientChannel.socket).thenAnswer((_: InvocationOnMock) => mockSocket)
    when(mockClientChannel.read(any(classOf[ByteBuffer]))).thenAnswer((params: InvocationOnMock) => {
      val buffer = params.getArgumentAt(0, classOf[ByteBuffer])
      val rawDat = currentMessage.getBytes("utf-8")
      buffer.put(rawDat)
      rawDat.length
    })

    var server: TCPServer = null

    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[TCPServer.TCPResponse] = {
      savedBuffers.append((currentAddress, buffers))
      if(messages.hasNext) {
        toReturn.clear()
        toReturn.add(placeholderMockReadable)
        val nextMessage = messages.next()
        currentAddress = nextMessage._1
        currentMessage = nextMessage._2
      } else {
        server.stop()
      }
      return None
    }

    server = new TCPServer(3000, () => handler, mockServerSocketChannel, mockSelector, _ => mockClientChannel,
      _ => mockReadable)

    toReturn.add(placeholderMockReadable)

    Await.result(server.start(), Duration(100, "millis"))

    val (goodAddr, goodBuffer) = savedBuffers(2)
    val (badAddr, badBuffer) = savedBuffers(1)

    assert(goodAddr equals mockAddress1)
    assert(badAddr equals mockAddress2)

    assert(new String(goodBuffer.last.array()).replace("\0", "") equals "HelloWorld")
    assert(new String(badBuffer.last.array()).replace("\0", "") equals "JUNK")
  }

  it should "write data to channels" in {
    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])
    val mockReadable = Mockito.mock(classOf[TCPServer.ClientSelectionKeyImpl])
    val mockSocket = Mockito.mock(classOf[Socket])
    val mockClientChannel = Mockito.mock(classOf[TCPServer.ClientSocketChannelImpl])
    val placeholderMockReadable = Mockito.mock(classOf[SelectionKey])

    val toReturn = new util.HashSet[SelectionKey]()
    val mockAddress = new InetSocketAddress(12345)
    val toWrite = ByteBuffer.wrap("HelloWorld".getBytes("utf-8"))
    var firstRead = true

    var server: TCPServer = null

    when(mockSelector.selectNow()).thenAnswer((_: InvocationOnMock) => toReturn.size)
    when(mockSelector.selectedKeys()).thenAnswer((_: InvocationOnMock) => toReturn.clone())
    when(mockReadable.isAcceptable).thenAnswer((_: InvocationOnMock) => false)
    when(mockReadable.isReadable).thenAnswer((_: InvocationOnMock) => if(firstRead){
      true
    }else{
      server.stop()
      false
    })
    when(mockReadable.channel).thenAnswer((_: InvocationOnMock) => null)
    when(mockSocket.isClosed).thenAnswer((_: InvocationOnMock) => false)
    when(mockClientChannel.socket).thenAnswer((_: InvocationOnMock) => mockSocket)
    when(mockClientChannel.isOpen).thenAnswer((_: InvocationOnMock) => true)
    when(mockClientChannel.getRemoteAddress).thenAnswer((_: InvocationOnMock) => mockAddress)
    when(mockClientChannel.read(any(classOf[ByteBuffer]))).thenAnswer((params: InvocationOnMock) => {
      val buffer = params.getArgumentAt(0, classOf[ByteBuffer])
      val rawDat = "junk".getBytes("utf-8")
      buffer.put(rawDat)
      firstRead = false
      rawDat.length
    })

    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[(ByteBuffer, Boolean)] = Some((toWrite, true))

    val handleProducer = Mockito.mock(classOf[() => TCPServer.TCPConsumer])
    when(handleProducer.apply()).thenAnswer((_: InvocationOnMock) => handler _)

    server = new TCPServer(3000, handleProducer, mockServerSocketChannel, mockSelector, _ => mockClientChannel,
      _ => mockReadable)

    toReturn.add(placeholderMockReadable)

    Await.result(server.start(), Duration(100, "millis"))
    verify(mockClientChannel, times(1)).write(toWrite)
    verify(mockClientChannel, times(1)).close()
  }

  it should "handle unexpected read closes" in {
    val mockSelector = Mockito.mock(classOf[Selector])
    val mockServerSocketChannel = Mockito.mock(classOf[TCPServer.AcceptingSocketChannelImpl])
    val mockReadable = Mockito.mock(classOf[TCPServer.ClientSelectionKeyImpl])
    val mockClientChannel = Mockito.mock(classOf[TCPServer.ClientSocketChannelImpl])
    val placeholderMockReadable = Mockito.mock(classOf[SelectionKey])
    val mockSocket = Mockito.mock(classOf[Socket])

    val toReturn = new util.HashSet[SelectionKey]()
    val mockAddress = new InetSocketAddress(12345)

    var checkCount = 10
    var socketOpen = true
    var server: TCPServer = null

    when(mockSelector.selectNow()).thenAnswer((_: InvocationOnMock) => {
      if(!socketOpen) {
        server.stop()
      } else if(checkCount == 0) {
        socketOpen = false
      } else {
        checkCount -= 1
      }
      toReturn.size
    })
    when(mockSelector.selectedKeys()).thenAnswer((_: InvocationOnMock) => toReturn)
    when(mockReadable.isAcceptable).thenAnswer((_: InvocationOnMock) => false)
    when(mockReadable.isReadable).thenAnswer((_: InvocationOnMock) => true)
    when(mockSocket.isClosed).thenAnswer((_: InvocationOnMock) => !socketOpen)
    when(mockClientChannel.socket).thenAnswer((_: InvocationOnMock) => mockSocket)
    when(mockClientChannel.getRemoteAddress).thenAnswer((_: InvocationOnMock) => mockAddress)
    when(mockClientChannel.read(any(classOf[ByteBuffer]))).thenAnswer((params: InvocationOnMock) => {
      val buffer = params.getArgumentAt(0, classOf[ByteBuffer])
      val rawDat = "Hello".getBytes("utf-8")
      buffer.put(rawDat)
      "Hello".length
    })

    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[(ByteBuffer, Boolean)] = None

    val handleProducer = Mockito.mock(classOf[() => TCPServer.TCPConsumer])
    when(handleProducer.apply()).thenAnswer((_: InvocationOnMock) => handler _)

    server = new TCPServer(3000, handleProducer, mockServerSocketChannel, mockSelector, _ => mockClientChannel,
      _ => mockReadable)

    toReturn.add(placeholderMockReadable)

    Await.result(server.start(), Duration(100, "millis"))

    val reqMapField = server.getClass.getDeclaredField("reqMap")
    reqMapField.setAccessible(true)
    val reqMap = reqMapField.get(server).asInstanceOf[mutable.HashMap[SocketAddress, TCPServer.ChannelBuffer]]
    assert(reqMap.isEmpty)
    verify(handleProducer, times(1)).apply()
  }

  it should "read data from real io ports" in {
    val data = "Hello World\n"
    var server: TCPServer = null
    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[TCPServer.TCPResponse] = {
      assert(new String(buffers.last.array()).replace("\0", "") equals data)
      server.stop()
      return None
    }
    val port = 3000
    server = new TCPServer(port, () => handler)
    val f = server.start()

    val clientSocket = new Socket("localhost", port)
    val outToServer = new DataOutputStream(clientSocket.getOutputStream)
    outToServer.write(data.getBytes("utf-8"))
    Await.result(f, Duration(100, "millis"))
  }

  it should "write data to real io ports" in {
    val data = "Hello World\n"
    var server: TCPServer = null
    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[TCPServer.TCPResponse] = {
      return Some(TCPServer.TCPResponse(ByteBuffer.wrap(data.getBytes("utf-8")), true))
    }
    val port = 3000
    server = new TCPServer(port, () => handler)
    val f = server.start()

    val clientSocket = new Socket("localhost", port)
    val outToServer = new DataOutputStream(clientSocket.getOutputStream)
    val inFromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))
    outToServer.write(" ".getBytes("utf-8"))
    val serverMessage = inFromServer.readLine() + "\n"
    server.stop()
    Await.result(f, Duration(100, "millis"))
    assert(serverMessage equals data)
  }

  it should "allocate more buffer space when needed" in {
    val data = "Hello World\n"
    var server: TCPServer = null
    var bufferCleared = false
    def handler(buffers: ArrayBuffer[ByteBuffer], bytesRead: Int): Option[TCPServer.TCPResponse] = {
      buffers.clear()
      bufferCleared = true
      return Some(TCPServer.TCPResponse(ByteBuffer.wrap(data.getBytes("utf-8")), false))
    }
    val port = 3000
    server = new TCPServer(port, () => handler)
    val f = server.start()

    val clientSocket = new Socket("localhost", port)
    val outToServer = new DataOutputStream(clientSocket.getOutputStream)
    val inFromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))
    outToServer.write(data.getBytes("utf-8"))
    val _ = inFromServer.readLine() + "\n"
    assert(bufferCleared)
    outToServer.write(data.getBytes("utf-8"))
    server.stop()
    Await.result(f, Duration(100, "millis"))
  }
}
