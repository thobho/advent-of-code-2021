package com.thobho

import com.thobho.Utils.readInputLines

case class Point(x: Int, y: Int)
case class Section(start: Point, end: Point) {
  def generateAllPoints(): List[Point] = {
    (start, end) match {
      case (start, end) if start.x == end.x =>
        if(start.y <= end.y) (start.y to end.y).map(Point(start.x, _)).toList
        else (start.y to end.y by -1).map(Point(start.x, _)).toList
      case (start, end) if start.y == end.y =>
        if(start.x <= end.x) (start.x to end.x).map(Point(_, start.y)).toList
        else (start.x to end.x by -1).map(Point(_, start.y)).toList
      case (start, end) =>
        if (start.x <= end.x && start.y <= end.y) ((start.x to end.x) zip (start.y to end.y)).map(xy => Point(xy._1, xy._2)).toList
        else if (start.x <= end.x && start.y >= end.y) ((start.x to end.x) zip (start.y to end.y by -1)).map(xy => Point(xy._1, xy._2)).toList
        else if (start.x >= end.x && start.y <= end.y) ((start.x to end.x by -1) zip (start.y to end.y)).map(xy => Point(xy._1, xy._2)).toList
        else if (start.x >= end.x && start.y >= end.y) ((start.x to end.x by -1) zip (start.y to end.y by -1)).map(xy => Point(xy._1, xy._2)).toList
        else Nil
    }
  }
}

object Day5 {
  private val inputPath = "resources/day-5-input.txt"

  private def readPoints():List[Section] = readInputLines(inputPath)
    .map(inputLine => inputLine.split(" -> "))
    .map(splitedLine => (splitedLine(0).split(","), splitedLine(1).split(",")))
    .map(coordinates => Section(
      Point(coordinates._1(0).toInt, coordinates._1(1).toInt),
      Point(coordinates._2(0).toInt, coordinates._2(1).toInt)))

  def solve(): Int = readPoints()
    .flatMap(_.generateAllPoints())
    .groupBy(x => x).view
    .mapValues(_.size)
    .count(pointWithOccurrences => pointWithOccurrences._2 >= 2)

  def main(args: Array[String]): Unit = {
    println(solve())
  }
}
