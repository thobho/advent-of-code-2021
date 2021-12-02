package com.thobho

import com.thobho.Utils.readInputLines

import scala.annotation.tailrec

case class Position(depth: Int = 0, distance: Int = 0, aim: Int = 0){
  def forward(steps:Int): Position = this.copy(depth + aim * steps, distance + steps)
  def down(steps:Int): Position = this.copy(depth, distance, aim + steps)
  def up(steps:Int): Position = this.copy(depth, distance, aim - steps)
}


object Day2 {

  val inputPath = "resources/day-2-input.txt"

  @tailrec
  def calculatePosition(steps: List[String], position: Position): Position = {
    steps match {
      case currentStep::rest => calculatePosition(rest, makeStep(currentStep, position))
      case Nil => position
    }
  }

  def makeStep(step: String, p: Position): Position = {
    val input = step.split(" ").toList
    input match {
      case "forward"::stepValue::Nil => p.forward(stepValue.toInt)
      case "down"::stepValue::Nil => p.down(stepValue.toInt)
      case "up"::stepValue::Nil => p.up(stepValue.toInt)
    }
  }

  def main(args: Array[String]): Unit = {
    val steps = readInputLines(inputPath)
    val finalPosition = calculatePosition(steps, Position())
    println(finalPosition)
    println(finalPosition.depth * finalPosition.distance)
  }

}
