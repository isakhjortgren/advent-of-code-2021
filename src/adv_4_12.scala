import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.Breaks._
object AllDone extends Exception { }
object adv_4_12 extends App {

  def getBoards(linesStr: List[String]): List[List[List[Int]]] = {
    val boards = linesStr.drop(1).sliding(6,6).map(_.drop(1))
    val boardsCleanedString = boards.map(_.map(x  => {
      val tmp = x.replace("  ", " ")
      tmp.charAt(0) match {
        case ' ' => tmp.drop(1)
        case _ => tmp
      }
    }))

    val boardsInts = boardsCleanedString.map(_.map(x => {
      x.split(" ").map(y => {
        val z: String = y
        z.toInt
      }).toList
    }).toList).toList
    boardsInts
  }

  def checkRow(row: List[Int], drawnValues: List[Int]): Boolean = {
    row.map(drawnValues.contains).reduce((a,b) => a && b)
  }

  def checkColumnI(board: List[List[Int]], ix: Int, drawnValues: List[Int]): Boolean = {
    val column = board.map(_(ix))
    checkRow(column, drawnValues)
  }
  def checkBingo(board: List[List[Int]], drawnValues: List[Int]) : Boolean = {
    val isRow = board.map(x=>checkRow(x, drawnValues)).reduce((a,b) => a || b)
    val isColumn = (0 until 5).map(ix => checkColumnI(board, ix, drawnValues)).reduce((a,b) => a || b)
    isColumn || isRow
  }

  def calculateScore(board: List[List[Int]], drawnValues: List[Int]): Int = {
    if (checkBingo(board, drawnValues)) {
      val boardScore = board.flatten.filter(x => !drawnValues.contains(x)).sum
      boardScore * drawnValues.last
    } else {
      -1
    }
  }

  def main_1(): Unit = {
    val src = Source.fromFile("./4-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()

    val numberSequence = linesStr.head.split(",").map(_.toInt)

    val boards = getBoards(linesStr)
    try {
      for (n <- 5 to numberSequence.length) {
        boards.foreach(x => {
          val score = calculateScore(x, numberSequence.take(n).toList)
          if (score > -1) {
            println(score)
            throw AllDone
          }

        })
      }
    } catch { case AllDone => println("Done")}
  }

  def main_2(): Unit = {
    val src = Source.fromFile("./4-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()

    val numberSequence = linesStr.head.split(",").map(_.toInt)

    val boards = getBoards(linesStr)
    var lastIncompleteScore = 0
    try {
      for (n <- 5 to numberSequence.length) {

        val scoreList = boards.map(x => calculateScore(x, numberSequence.take(n).toList))
        if (scoreList.count(x => -1 < x) == boards.length) {
          val prevScoreList = boards.map(x => calculateScore(x, numberSequence.take(n-1).toList))
          (scoreList zip prevScoreList).foreach(x => {
            if (x._2 == -1) {
              println(x._1)
            }
          })
          throw AllDone
        } else {
          lastIncompleteScore = scoreList.filter(x => -1 < x).sum
        }
      }
    } catch { case AllDone => println("Done")}
  }
  main_2()
}