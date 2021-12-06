package com.thobho

import com.thobho.Utils.{readInputIntLines, readInputLines}

import scala.annotation.tailrec

object Day6 {
  private val inputPath = "resources/day-6-input.txt"
  private val simulationTime = 150

  @tailrec
  private def simulateLanterfish(fishCounters: List[Int], day: Int = 0): (List[Int], Int) = {
    if(day == simulationTime) (fishCounters, day)
    else {
      val fishToAdd = fishCounters.count(_ == 0)
      val nexDayFish = fishCounters.map(counter => if (counter == 0) 6 else counter - 1)
      simulateLanterfish(nexDayFish ++ List.fill(fishToAdd)(8), day + 1)
    }
  }


  def main(args: Array[String]): Unit = {
    val initialState = readInputLines(inputPath).head.split(",").map(_.toInt).toList
    println(simulateLanterfish(initialState)._1.size)
  }
}
