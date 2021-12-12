import scala.annotation.tailrec
import scala.io.Source

object adv_11_12 extends App {


  @tailrec
  def performFlash(energyLevels: List[List[Int]], previousIndices: Set[(Int, Int)]): (List[List[Int]], Int) = {

    val flashingIndices = energyLevels.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(col => (row._2, col._2, col._1 > 9)))
    val indices = flashingIndices.filter(_._3).map(x => (x._1, x._2)).toSet
    val newIndices = indices.diff(previousIndices)

    if (newIndices.isEmpty) {
      val newEnergyLevels = energyLevels.map(_.map(x => {
        if (x > 9) 0 else x
      }))
      (newEnergyLevels, indices.size)
    } else {
      val coords = List((1,1), (1,0), (1,-1), (0,1), (0,-1), (-1,1), (-1,0), (-1,-1))
      val updatedEnergyLevels = energyLevels.zipWithIndex.map(row => {
        row._1.zipWithIndex.map(col => {
          val neighbouringLights = coords.map(diff => newIndices.contains((row._2+diff._1, col._2+diff._2))).count(_ == true)
          col._1 + neighbouringLights
        })
      })
      performFlash(updatedEnergyLevels, indices)
    }
  }

  def oneStep(energyLevels: List[List[Int]]): (List[List[Int]], Int) = {
    val energyStep1 = energyLevels.map(_.map(_+1))
    performFlash(energyStep1, Set.empty)
  }

  def main1(): Unit = {
    val src = Source.fromFile("./11-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val energyLevels = linesStr.map(_.toList.map(_.asDigit))
    val endResult = (1 to 100).foldLeft((energyLevels, 0))((x, _)=> {
      val res = oneStep(x._1)
      (res._1, res._2 + x._2)
    })
    println(endResult._2)
  }

  def main2(): Unit = {
    val src = Source.fromFile("./11-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    var energyLevels = linesStr.map(_.toList.map(_.asDigit))

    var firstAttempt: Int = -1
    var loopix = 0
    while (firstAttempt < 0) {
      val res = oneStep(energyLevels)
      loopix += 1
      energyLevels = res._1
      if (res._2 == 100) {
        firstAttempt = loopix
        println(loopix)
      }
    }

  }
  main2()
}
