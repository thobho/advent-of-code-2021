package com.thobho

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

case class Bingo(){

}

object Day4 {

  private val boardSize = 5
  private val inputPath = "resources/day-4-input.txt"

  case class Cell(value:Int, row: Int, column:Int, marked: Boolean = false)

  case class Board(cells: Set[Cell]) {
    def isWinningBoard: Boolean = checkWinning(_.row) || checkWinning(_.column)

    private def checkWinning[K](fun: Cell => K) = cells.groupBy(fun)
      .map(seqWithCells => seqWithCells._2.filter(_.marked))
      .map(_.size)
      .exists(_ == boardSize)

    def calculateSumOfNotMarked(): Int = cells.filter(!_.marked).map(_.value).sum

    def markValue(valueToFind: Int): Board = {
      Board(cells.map(cell => if(cell.value == valueToFind) cell.copy(marked = true) else cell))
    }
  }

  private val processLine = (lineWithRow: (String, Int)) => lineWithRow match {
    case (line, rowIndex) => line.trim
      .split("\\s+").toList
      .zipWithIndex
      .map((valueWithColumn: (String, Int)) => Cell(valueWithColumn._1.toInt, rowIndex, valueWithColumn._2))
  }

  def createBoard(inputLines:List[String]): Board = {
    Board(
      inputLines.tail
        .zipWithIndex
        .flatMap(processLine)
        .toSet
    )
  }

  def readBoards():(List[Int], List[Board]) = Using(Source.fromFile(inputPath)) { source =>
      source.getLines().toList match {
        case numberLine::boardLines => (
          numberLine.split(",").map(_.toInt).toList,
          boardLines.sliding(6, 6).map(createBoard).toList
        )
    }
  }.get

  type SearchParams = (List[Int], Int, List[Board], List[Board])

  @tailrec
  def findRec(params: SearchParams): SearchParams = {
    val (numberToCrossOut, currentNumber, notWining, winning) = params
    if(notWining.isEmpty){ //winning.size == 1 for part one of exercise
       (numberToCrossOut, currentNumber, notWining, winning)
    } else {
      val (newWinningBoards, newNotWiningBoards) = crossFromBoards(numberToCrossOut.head, notWining)
      findRec(numberToCrossOut.tail, numberToCrossOut.head, newNotWiningBoards, newWinningBoards)
    }
  }

  private def crossFromBoards(numberToCross: Int, boards: List[Board]) =
    boards.map(board => board.markValue(numberToCross)).partition(_.isWinningBoard)

  def main(args: Array[String]): Unit = {
    val (numbers, boards) = readBoards()
    val result = findRec((numbers, -1, boards, Nil))
    println(result._4.last.calculateSumOfNotMarked() * result._2)
  }
}
