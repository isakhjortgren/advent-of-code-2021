import scala.annotation.tailrec
import scala.io.Source

object adv_12_12 extends App {

  def isLargeCave(caveName: String): Boolean = {
    caveName.map(_.isUpper).reduce(_ && _)
  }

  def findPath(caveConnections: List[(String, String)], currentNode: String, visitedSmallCaves: Set[String]): List[List[String]] = {

    val potentialNextSteps = caveConnections.filter(connection => connection._1 == currentNode).filter(connection => !visitedSmallCaves.contains(connection._2))

    potentialNextSteps.flatMap(connection => {
      if (connection._2 == "end") {
        List(List("end"))
      } else {
        val updatedVisitedSmallCaves = {
          if (isLargeCave(connection._2)) {
            visitedSmallCaves
          } else {
            visitedSmallCaves.union(Set(connection._2))
          }
        }
        val upcomingPaths = findPath(caveConnections, connection._2, updatedVisitedSmallCaves)
        upcomingPaths.map(newPath => List(connection._1) ++ newPath)
      }
    })
  }

  def main1(): Unit = {
    val src = Source.fromFile("./12-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val caveConnections = linesStr.map(x => x.split("-")).flatMap(x => List((x(0), x(1)), (x(1), x(0))))
    val paths = findPath(caveConnections, "start", Set("start"))
    println(paths.length)
  }

  def findPath2(caveConnections: List[(String, String)], currentNode: String, visitedSmallCaves: Set[String], extraNode: Set[String]): List[List[String]] = {

    val potentialNextSteps = caveConnections.filter(connection => connection._1 == currentNode).filter(connection => {
      (!visitedSmallCaves.contains(connection._2)) || extraNode.isEmpty
    }).filter(connection => connection._2 != "start")

    potentialNextSteps.flatMap(connection => {
      if (connection._2 == "end") {
        List(List("end"))
      } else {
        val updatedVisitedSmallCaves = {
          if (isLargeCave(connection._2)) {
            visitedSmallCaves
          } else {
            visitedSmallCaves.union(Set(connection._2))
          }
        }
        val updExtraNode: Set[String] = {
          if (visitedSmallCaves.contains(connection._2)) {
            Set(connection._2)
          } else {
            extraNode
          }
        }
        val upcomingPaths = findPath2(caveConnections, connection._2, updatedVisitedSmallCaves, updExtraNode)
        upcomingPaths.map(newPath => List(connection._1) ++ newPath)
      }
    })
  }

  def main2(): Unit = {
    val src = Source.fromFile("./12-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val caveConnections = linesStr.map(x => x.split("-")).flatMap(x => List((x(0), x(1)), (x(1), x(0)))).filter(x => (x._2 != "start") || (x._1 != "end"))
    val paths = findPath2(caveConnections, "start", Set("start"), Set.empty)
    println(paths.length)
    println()
  }
  main2()
}