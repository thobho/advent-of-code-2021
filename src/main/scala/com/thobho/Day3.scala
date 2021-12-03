package com.thobho

import com.thobho.Utils.readInputLines

import java.util.function.BiFunction

object Day3 {

  private val inputPath = "resources/day-3-input.txt"
  private val sequenceLength = 12
  private val emptyCounts = List.fill(sequenceLength)(0)

  def addBitsToBitCountList(sequence: String, bitCounts:List[Int], bitToCount: Char): List[Int] =
    sequence.toCharArray.zip(bitCounts)
      .map { case (bit, count) => if (bit == bitToCount) count + 1 else count}
      .toList

  def countBits(sequences: List[String], bitToCount: Char): List[Int] =
    sequences.foldLeft(emptyCounts)((counts, nextSequence) => addBitsToBitCountList(nextSequence, counts, bitToCount))

  def generateBinary(input: List[String], operation: BiFunction[Int, Int, Boolean]): Int = {
    val binaryAsString = countBits(input, '1').zip(countBits(input, '0'))
      .map(oneAndZeroCounts => if (operation(oneAndZeroCounts._1, oneAndZeroCounts._2)) "1" else "0")
      .reduce(_+_)
    toDecimal(binaryAsString)
  }

  def toDecimal(binaryString: String): Int = Integer.parseInt(binaryString, 2)

  def main(args: Array[String]): Unit = {
    val input = readInputLines(inputPath)
    val gamma = generateBinary(input, _ > _)
    val kappa = generateBinary(input, _ < _)
    print(gamma * kappa)
  }
}
