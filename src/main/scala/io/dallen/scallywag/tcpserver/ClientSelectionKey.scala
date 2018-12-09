package io.dallen.scallywag.tcpserver

import java.nio.channels.{SelectableChannel, Selector}

trait ClientSelectionKey {
  def isAcceptable: Boolean
  def isReadable: Boolean
  def channel: SelectableChannel
  def selector: Selector
}
