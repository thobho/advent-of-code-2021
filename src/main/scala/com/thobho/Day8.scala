package com.thobho

import Utils.readInputLines


object Day8 {
  private val inputPath = "resources/day-8-input.txt"


  val segmentNumberEncoding = Map(
    0 -> List(0,1,2,4,5,6),
    1 -> List(5,6),
    2 -> List(1,2,3,4,5),
    3 -> List(2,3,4,5,6),
    4 -> List(0,3,5,6),
    5 -> List(0,2,3,4,6),
    6 -> List(0,1,2,3,4,6),
    7 -> List(2,5,6),
    8 -> List(0,1,2,3,4,5,6),
    9 -> List(0,2,3,4,5,6)
  )

  def calculatePermutation(permutation: List[Char], inputCodes: Set[String]): Boolean = {
    val permutationEncoded = segmentNumberEncoding.values
      .map(encodedSegments => encodedSegments.map(permutation(_)))
      .map(charArr => charArr.mkString)
      .toSet
    val a = permutationEncoded.map(code => code.toSet)
    val b = inputCodes.map(code => code.toSet)
    a.equals(b)
  }

  def matchToPermutation(codes: Set[String]): List[Char] = {
      "abcdefg".toList.permutations
        .find(perm => calculatePermutation(perm, codes))
        .get
  }

  def decodeCipher(key:List[Char], input: String):String =
    segmentNumberEncoding.view
      .mapValues(encoding => encoding.map(key(_)).mkString)
      .toMap
      .filter(tuple => tuple._2.toSet == input.toSet)
      .head._1.toString

  def main(args: Array[String]): Unit = {
    val result = readInputLines(inputPath)
      .map(line => line.split(" \\| "))
      .map(splitedArr => (splitedArr(0), splitedArr(1)))
      .map { case (encoding, digits) => (encoding.split(" "), digits.split(" "))}
      .map { case (encoding, digits) => (encoding.toSet, digits.toList)}
      .map { case (encoding, digits) => (matchToPermutation(encoding), digits)}
      .map { case (decoded, digits) => digits.map(digit => decodeCipher(decoded, digit))}
      .map { digits => digits.reduce(_+_)}
      .map(_.toInt)
      .sum

    println(result)
  }
}
