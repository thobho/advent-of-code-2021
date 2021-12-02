package com.thobho

import com.thobho.Day1.inputPath

import scala.io.Source
import scala.util.Using

object Utils {

  def readInputLines(inputPath: String): List[String] = {
    Using(Source.fromFile(inputPath)) { source =>
      source.getLines().toList
    }.get
  }

  def readInputIntLines(inputPath: String): List[Int] = {
    readInputLines(inputPath).map(number => number.toInt)
  }
}
