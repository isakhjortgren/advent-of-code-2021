import scala.annotation.tailrec
import scala.io.Source
object adv_7_12 extends App {

  def getIndividual(x: Int, value: Int): Int = {
    val tmp = 1 to (x-value).abs
    tmp.sum
  }

  def getScore2(hori: List[Int], value: Int): Int = {
    hori.map(x => getIndividual(x, value)).sum
  }

  def getScore(hori: List[Int], value: Int): Int = {
    hori.map(x => (x-value).abs).sum
  }

  @tailrec
  def intervalHalvation(hori: List[Int], a: Int, b: Int): (Int, Int) = {
    if ((b-a) <= 1) {
      (a, b)
    } else {
      val proposal: Int = (a+b)/2
      if (getScore2(hori, a) < getScore2(hori, b)) {
        intervalHalvation(hori, a, proposal)
      } else {
        intervalHalvation(hori, proposal, b)
      }
    }
  }

  def main1(): Unit = {
    val src = Source.fromFile("./7-12/input.txt")
    val hori = src.getLines.toList.head.split(",").map(_.toInt).toList
    src.close()

    val a = hori.min
    val b = hori.max

    val res = intervalHalvation(hori, a, b)
    println(getScore2(hori, res._1))
    println(getScore2(hori, res._2))
  }
  main1()
}