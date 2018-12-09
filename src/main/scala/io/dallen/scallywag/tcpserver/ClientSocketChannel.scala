package io.dallen.scallywag.tcpserver

import java.net.{Socket, SocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.Selector

trait ClientSocketChannel {
  def accept(block: Boolean, selector: Selector): ClientSocketChannel
  def getRemoteAddress: SocketAddress
  def isOpen: Boolean
  def read(buffer: ByteBuffer): Int
  def write(buffer: ByteBuffer): Int
  def close(): Unit
  def socket: Socket
}
