package com.thobho

import com.thobho.Utils.readInputLines

import scala.annotation.tailrec

object Day6 {
  private val inputPath = "resources/day-6-input.txt"
  private val simulationTime = 256

  @tailrec
  private def simulateSlow(fishCounters: List[Int], day: Int = 0): (List[Int], Int) = {
    day match {
      case _ if day == simulationTime => (fishCounters, day)
      case _ =>
        val fishToAdd = fishCounters.count(_ == 0)
        val nexDayFish = fishCounters.map(counter => if (counter == 0) 6 else counter - 1)
        simulateSlow(nexDayFish ++ List.fill(fishToAdd)(8), day + 1)
    }
  }

  @tailrec
  def simulateFast(fishCounters: Map[Int, BigInt], day:Int = 0): (Map[Int, BigInt], Int) = {
    day match {
      case _ if day == simulationTime => (fishCounters, day)
      case _ =>
        val newFish = (0 to 8).map {
          case fishNumber@8 => (fishNumber, fishCounters(0))
          case fishNumber@6 => (fishNumber, fishCounters(7) + fishCounters(0))
          case fishNumber => (fishNumber, fishCounters(fishNumber + 1))
        }.toMap
        simulateFast(newFish, day + 1)
    }
  }

  def buildCountMaps(input: List[Int]):Map[Int, BigInt] = {
    val inputMap = input.groupBy(x => x).view.mapValues(_.size).toMap
    (0 to 8).map(fishNumber => (fishNumber, BigInt(inputMap.getOrElse(fishNumber, 0)))).toMap
  }

  def main(args: Array[String]): Unit = {
    val initialState = readInputLines(inputPath).head.split(",").map(_.toInt).toList
    val results = simulateFast(buildCountMaps(initialState))
    println(results._1.values.sum)
  }
}
