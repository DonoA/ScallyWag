package io.dallen.scallywag.tcpserver

import java.net.InetSocketAddress
import java.nio.channels.Selector

trait AcceptingSocketChannel {
  def open(inetSocketAddress: InetSocketAddress, selector: Selector)
  def close(): Unit
}
