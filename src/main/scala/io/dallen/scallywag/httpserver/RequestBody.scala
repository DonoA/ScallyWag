package io.dallen.scallywag.httpserver

abstract class RequestBody()

case class RawBody(data: Array[Byte]) extends RequestBody
case class StringBody(data: String) extends RequestBody
case class FormBody(data: Map[String, String]) extends RequestBody
