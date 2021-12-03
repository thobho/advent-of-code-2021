package com.thobho

import com.thobho.Day3.{countBits, generateBinary, inputPath, main, toDecimal}
import com.thobho.Utils.readInputLines

import java.util.function.BiFunction
import scala.annotation.tailrec

object Day3Part2 {

  private val inputPath = "resources/day-3-input.txt"

  @tailrec
  def determineColumn(binaryStrings: List[String], position: Int, operation: BiFunction[Int, Int, Boolean]): List[String] = {
        binaryStrings match {
          case head::Nil => List(head)
          case _ =>
            val zerosCount = countBits(binaryStrings, '0')(position)
            val onesCount = countBits(binaryStrings, '1')(position)
            val searchedValues = if(operation(zerosCount, onesCount)) '0' else '1'
            val result = binaryStrings.filter(binaryString => hasBitOnPosition(binaryString, position, searchedValues))
            determineColumn(result, position + 1, operation)
        }
  }

  private def hasBitOnPosition(binaryString: String, position: Int, char: Char):Boolean = binaryString.charAt(position) == char

  def main(args: Array[String]): Unit = {
    val input = readInputLines(inputPath)
    val oxygen = determineColumn(input, 0, _ > _).head
    val co2 = determineColumn(input, 0, _ < _).head
    println(toDecimal(oxygen) * toDecimal(co2))
  }
}
