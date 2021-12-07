package com.thobho

import com.thobho.Utils.readInputLines

object Day7 {

  private val inputPath = "resources/day-7-input.txt"

  def solve(input:List[Int], fuelFun: (Int, Int) => Int):Int =
    (input.min to input.max).map(target => input.map(number => fuelFun(target, number)).sum).min

  val constant: (Int, Int) => Int = (target, position) => Math.abs(target - position)
  val linear: (Int, Int) => Int = (target, position) => ((1 + Math.abs(target - position)) * Math.abs(target - position)) / 2


  def main(args: Array[String]): Unit = {
    val input = readInputLines(inputPath).head.split(",").toList.map(_.toInt)
    val solutionOne = solve(input, constant)
    val solutionTwo = solve(input, linear)
    println(solutionOne)
    println(solutionTwo)
  }

}
