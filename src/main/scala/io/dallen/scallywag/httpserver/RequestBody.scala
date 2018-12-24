package io.dallen.scallywag.httpserver

import java.io.{File, FileOutputStream}

import scala.collection.mutable.ArrayBuffer

abstract class RequestBody()

object RequestBody {
  case class RawBody(data: Array[Byte]) extends RequestBody
  case class StringBody(body: String) extends RequestBody
  case class FormBody(private val data: Map[String, ArrayBuffer[FormData]]) extends RequestBody {
    def get(name: String): Option[ArrayBuffer[FormData]] = data.get(name)
    def apply(name: String): ArrayBuffer[FormData] = data(name)
  }

  abstract class FormData()
  case class FormString(body: String) extends FormData
  case class FormFile(format: String, fileName: String, private val data: Array[Byte]) extends FormData {
    def getBytes: Array[Byte] = data
    def save(file: File): Unit = {
      try {
        val fos = new FileOutputStream(file)
        try
          fos.write(data)
        finally if (fos != null) fos.close()
      }
    }
  }
}




