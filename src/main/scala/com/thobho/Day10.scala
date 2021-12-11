package com.thobho

import com.thobho.Day10.Token.{closeToOpenPair, correctTokenPoints, openToClosePair, wrongTokenPoints}

import scala.annotation.tailrec

object Day10 {

  private val inputPath = "resources/day-10-input.txt"

  object Token {
    val closeToOpenPair = Map('}' -> '{', ']' -> '[', ')' -> '(', '>' -> '<')
    val openToClosePair: Map[Char, Char] = closeToOpenPair.map { case (k, v) => (v, k) }
    val wrongTokenPoints = Map(')' -> BigInt(3), ']' -> BigInt(57), '}' -> BigInt(1197), '>' -> BigInt(25137))
    val correctTokenPoints = Map(')' -> BigInt(1), ']' -> BigInt(2), '}' -> BigInt(3), '>' -> BigInt(4))
  }

  case class Token(value: Char) {
    def isOpening: Boolean = openToClosePair.keys.toList.contains(value)

    def isClosing: Boolean = !isOpening

    def matchesStart(startTag: Token): Boolean = closeToOpenPair(this.value) == startTag.value
  }


  @tailrec
  def parseBrackets(input: List[Token], stack: List[Token] = Nil, current: Option[Char] = None): (List[Token], List[Token], Option[Char]) =
    input match {
      case Nil => {
        (input, stack, current)
      }
      case head :: tail =>
        if (head.isOpening) parseBrackets(tail, head :: stack, Some(head.value))
        else if (head.isClosing && head.matchesStart(stack.head)) {
          parseBrackets(tail, stack.tail, Some(head.value))
        } else (tail, stack, Some(head.value))
    }

  def solvePartOne(input: List[List[Token]]): BigInt = {
    input
      .map(parseBrackets(_))
      .filter { case (rest, _, _) => rest.nonEmpty }
      .map { case (_, _, current) => current }
      .map(wrongToken => wrongTokenPoints(wrongToken.get))
      .sum
  }

  def solvePartTwo(input: List[List[Token]]): BigInt = {
    val result = input
      .map(parseBrackets(_))
      .filter { case (rest, _, _) => rest.isEmpty }
      .map { case (_, stack, _) => stack.map(token => openToClosePair(token.value)) }
      .map { tokens => tokens.map(correctTokenPoints(_)).reverse }
      .map { points => points.foldRight(BigInt(0))((n, acc) => acc * 5 + n) }



    oddLengthListMedian(result)
  }

  def oddLengthListMedian(list: List[BigInt]): BigInt = {
    list.sortWith(_ < _).drop(list.length/2).head
  }

  def main(args: Array[String]): Unit = {
    val tokens = Utils.readInputLines(inputPath)
      .map(inputLine => inputLine.map(Token(_)).toList)

    println(solvePartOne(tokens))
    println(solvePartTwo(tokens))
  }
}
