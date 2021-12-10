package com.thobho

object Day9 {
  private val inputPath = "resources/day-9-input.txt"

  object Cell{
    type NeighbourFinder = (input: Array[Array[Int]], rowIndex: Int, columnIndex:Int) => List[Int]
    private val bottom: NeighbourFinder = (input, rowIndex, columnIndex) =>
      if(rowIndex < input.length - 1) List(input(rowIndex + 1)(columnIndex)) else Nil
    private val upper: NeighbourFinder = (input, rowIndex, columnIndex) =>
      if(rowIndex != 0) List(input(rowIndex - 1)(columnIndex)) else Nil
    private val left: NeighbourFinder = (input, rowIndex, columnIndex) =>
      if(columnIndex != 0) List(input(rowIndex)(columnIndex - 1)) else Nil
    private val right: NeighbourFinder = (input, rowIndex, columnIndex) =>
      if(columnIndex < input(0).length - 1) List(input(rowIndex)(columnIndex + 1)) else Nil

    val combinedFinders: NeighbourFinder = (input, rowIndex, columnIndex) =>
      List(upper, bottom, left, right).flatMap(_(input, rowIndex, columnIndex))
  }

  case class Cell(row: Int, column: Int, value: Int, neighbourValues: List[Int])

  def createCell(input: Array[Array[Int]], rowIndex: Int, columnIndex:Int): Cell =
    Cell(rowIndex, columnIndex, input(rowIndex)(columnIndex), Cell.combinedFinders(input, rowIndex, columnIndex))

  def createCells(input: Array[Array[Int]]): IndexedSeq[Cell] =
    for {
      rowIndex <- input.indices
      columnIndex <- input(0).indices
    } yield createCell(input, rowIndex, columnIndex)

  def main(args: Array[String]): Unit = {
    val input = Utils.readInputLines(inputPath).toArray
      .map(row => row.map(_.toString.toInt).toArray)

    val result = createCells(input)
      .filter(cell => cell.neighbourValues.map(nv => nv > cell.value).reduce(_ && _))
      .map(cell => cell.value + 1)
      .sum

    println(result)
  }
}
