package com.thobho

object Day9 {
  private val inputPath = "resources/day-9-input.txt"

  type SeaFlorMap = Array[Array[Int]]
  type Position = (Int, Int)
  type CellValue = Int
  type Cell = (Position, CellValue)
  type CellWithNeighbours = (Position,CellValue,List[Cell])
  type NeighbourFinder = (input: SeaFlorMap, r: Int, c:Int) => List[Cell]

  val bottom: NeighbourFinder = (input, r, c) =>
    if(r < input.length - 1) List(((r + 1, c), (input(r + 1)(c)))) else Nil
  val upper: NeighbourFinder = (input, r, c) =>
    if(r != 0) List((( r - 1, c), input(r - 1)(c))) else Nil
  val left: NeighbourFinder = (input, r, c) =>
    if(c != 0) List(((r, c - 1), input(r)(c - 1))) else Nil
  val right: NeighbourFinder = (input, r, c) =>
    if(c < input(0).length - 1) List(((r, c + 1), input(r)(c + 1))) else Nil

  def findNeighbours(input: SeaFlorMap, position: Position): List[Cell] =
      List(upper, bottom, left, right).flatMap(_(input, position._1, position._2))


  def createCells(input: SeaFlorMap): IndexedSeq[CellWithNeighbours] =
    for {
      rowIndex <- input.indices
      columnIndex <- input(0).indices
    } yield ((rowIndex, columnIndex), input(rowIndex)(columnIndex), findNeighbours(input, (rowIndex, columnIndex)))

  def searchLowPoints(input: SeaFlorMap): List[CellWithNeighbours] = createCells(input).toList
    .filter {
      case (_, cellValue, neighbour) => neighbour.map {
        case (_,neighbourValue) => neighbourValue > cellValue
      }.reduce(_ && _)
    }



  def searchBasins(input: SeaFlorMap, lowPoints: List[CellWithNeighbours]): List[(Int, Int)] = {
    Nil
  }


  def searchBasin(seeFloor: SeaFlorMap, current: Position, basin: Set[Position] = Set.empty, visited: Set[Position] = Set.empty) : (Set[Position], Set[Position]) = {
    val (r,c) = current
    val currentValue = seeFloor(r)(c)

    val neighbours = findNeighbours(seeFloor, current)

    val basinPart = neighbours
      .filter { case (_, neighbourValue) => neighbourValue > currentValue }
      .filter { case (_, value) => value != 9}
      .map { case (position, _) => position}
      .toSet

    val newVisited = neighbours
      .map { case (position, _) => position}
      .toSet

    val neighboursToSearch = basinPart &~ visited

    if(neighboursToSearch.isEmpty) (basin, visited)
    else {
      neighboursToSearch
        .map(nextNeighbour => searchBasin(seeFloor, nextNeighbour, basinPart | basin + current, visited | newVisited))
        .reduce((left, right) => (left._1 | right._1, left._2 | right._2))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.readInputLines(inputPath).toArray
      .map(row => row.map(_.toString.toInt).toArray)

    val result = searchLowPoints(input)
      .map { case (position, _, _) => position }
      .map (searchBasin(input, _))
      .map (_._1)
      .map (_.size)
      .sortWith(_ > _)
      .take(3)
      .product

    print(result)
  }
}
