import scala.io.Source

object adv_13_12 extends App {

  def foldAlong(point: Int, foldNumber: Int): Int = {
    val pointMod = point % (2*(foldNumber+1))
    val pointMirror = {
      if (pointMod > foldNumber) {
        2*foldNumber - pointMod
      } else {
      pointMod
      }
    }
    pointMirror
  }

  def main1(): Unit = {
    val src = Source.fromFile("./13-12/input.txt")
    val dots = src.getLines.toList.map(x => x.split(",").map(_.toInt)).map(y => (y(0), y(1)))
    src.close()
    val fold_x = 655

    val afterFold = dots.map(x => (foldAlong(x._1, fold_x), x._2))
    println(afterFold.toSet.size)
  }

  def main2(): Unit = {
    val src = Source.fromFile("./13-12/input.txt")
    val dots = src.getLines.toList.map(x => x.split(",").map(_.toInt)).map(y => (y(0), y(1)))
    src.close()
    val srcInstructions = Source.fromFile("./13-12/input-folds.txt")
    val instructions = srcInstructions.getLines.toList.map(line => line.drop(11).split("=")).map(res => (res(0), res(1).toInt))
    val foldingResult = instructions.foldLeft(dots.toSet)((dotSet, instruction) => {
      dotSet.map(point => {
        if (instruction._1 == "x") {
          (foldAlong(point._1, instruction._2), point._2)
        } else {
          (point._1, foldAlong(point._2, instruction._2))
        }
      })
    })

    val maxX = foldingResult.map(_._1).max + 2
    val maxY = foldingResult.map(_._2).max + 2
    val matrix = (0 to maxY).map(iRow => (0 to maxX).map(iCol => foldingResult.contains((iCol, iRow))).toList).toList
    matrix.foreach{row => {
      println(row.map(x => if (x) "x" else " ").reduce(_ + _))
    }}
  }

  main2()


}
//fold3
//0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 -  16 17 18 19 20
//0 1 2 - 4 5 6 - 8 9 10 -  12 13 14 -  16 17 18 -  20
//mod 8
//0 1 2 3 4 5 6 7 0 1 2  3  4  5  6  7  0  1  2  3  4
//0 1 2 3 2 1 0 -
