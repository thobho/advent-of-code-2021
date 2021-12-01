package com.thobho

import com.thobho.Utils.readInput

import scala.annotation.tailrec
import scala.io.Source

//https://adventofcode.com/2021/day/1
object Day1 {

  val inputPath = "resources/day-1-input.txt"

  @tailrec
  def solve(count: Int) (input: List[Int]): (List[Int], Int) = {
    input match {
      case first :: second :: tail => solve(if (second > first) count + 1 else count)(second :: tail)
      case _ :: Nil => (Nil, count)
    }
  }

  //part two of exercise
  def windowedSum(input: List[Int]): List[Int] = input.sliding(3).map(window => window.sum).toList
  def solvePatTwo: List[Int] => (List[Int], Int) = windowedSum _ andThen solve(0)

  def main(args: Array[String]): Unit = {
    val input = readInput()
    val partOneResult = solve(0)(input)._2
    val partTwoResult = solvePatTwo(input)._2
    println(partOneResult)
    println(partTwoResult)
  }
}
