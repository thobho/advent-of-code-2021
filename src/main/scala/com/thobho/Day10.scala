package com.thobho

import com.thobho.Day10.Token.{closeToOpenPair, openToClosePair, openingTokes, points}

import scala.annotation.tailrec

object Day10 {

  private val inputPath = "resources/day-10-input.txt"


  object Token {
    val openingTokes = List('{','(','[','<')
    val closeToOpenPair = Map(
      '}' -> '{',
      ']' -> '[',
      ')' -> '(',
      '>' -> '<'
    )
    val openToClosePair: Map[Char, Char] = closeToOpenPair
      .map {case (k,v) => (v,k)}

    val points = Map(
    ')'-> 3,
    ']'-> 57,
  '}'-> 1197,
  '>'-> 25137
    )
  }

  case class Token(value: Char) {
    def isOpening:Boolean = openingTokes.contains(value)
    def isClosing:Boolean = !isOpening
    def matchesStart(startTag: Token): Boolean = closeToOpenPair(this.value) == startTag.value
  }


  @tailrec
  def parseBrackets(input: List[Token], stack: List[Token] = Nil, current:Option[Char] = None): (List[Token], List[Token], Option[Char]) =
    input match {
      case Nil => (Nil,Nil, current)
      case head::tail =>
        if(head.isOpening) parseBrackets(tail, head :: stack, Some(head.value))
        else if(head.isClosing && head.matchesStart(stack.head)) {
          parseBrackets(tail, stack.tail, Some(head.value))
        } else (tail, stack, Some(head.value))
    }

  def solvePartOne(input:List[List[Token]]): Int = {
    input
      .map(parseBrackets(_))
      .filter {case (_, stack, _) => stack.nonEmpty}
      .map {case (_, _, current) => current}
      .map(wrongToken => points(wrongToken.get))
      .sum
  }

  def solvePartTwo(input:List[List[Token]]): Unit = {
    val result = input
      .map(parseBrackets(_))
      .filter {case (_, stack, _) => stack.isEmpty}

    println(result)
  }

  def main(args: Array[String]): Unit = {
    val tokens = Utils.readInputLines(inputPath)
      .map(inputLine => inputLine.map(Token(_)).toList)


    solvePartTwo(tokens)
//    print(solvePartOne(tokens))
  }
}
