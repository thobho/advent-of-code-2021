package com.thobho

import com.thobho.Day1.inputPath

import scala.io.Source

object Utils {
  def readInput(): List[Int] = {
    val source = Source.fromFile(inputPath)
    val result = source.getLines().map(number => number.toInt).toList
    source.close()
    result
  }
}
