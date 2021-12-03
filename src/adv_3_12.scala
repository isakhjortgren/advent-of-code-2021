import scala.annotation.tailrec
import scala.io.Source

object adv_3_12 extends App {

  def binaryConv(a:(Int, Int)): Int = {
    a._1*scala.math.pow(2, a._2).toInt
  }

  def main_1(): Unit = {
    val src = Source.fromFile("../3-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val lines = linesStr.map(_.toList.map(_.asDigit))
    println(lines)
    val reduced = lines.reduce((a, b) => (a zip b).map(x => x._1 + x._2))
    println(reduced)
    val gamma = reduced.map(x => {if (x+x > lines.length) {1} else {0}})
    val epsilon = reduced.map(x => {if (x+x > lines.length) {0} else {1}})
    println(gamma)
    println(epsilon)
    val epsilonValue = epsilon.reverse.zipWithIndex.map(binaryConv).sum
    val gammaValue = gamma.reverse.zipWithIndex.map(binaryConv).sum
    println(epsilonValue*gammaValue)
  }

  @tailrec
  def oxyRec(x: List[List[Int]], ix: Int): List[List[Int]] = {
    if (x.length == 1) {
      x
    } else {
      val items_with_ones = x.filter(_(ix) == 1)
      if (2*items_with_ones.length >= x.length) {
        oxyRec(items_with_ones, ix+1)
      } else {
        val tmp = x.filter(_(ix) == 0)
        oxyRec(tmp, ix+1)
      }
    }
  }

  @tailrec
  def co2Rec(x: List[List[Int]], ix: Int): List[List[Int]] = {
    if (x.length == 1) {
      x
    } else {
      val items_with_ones = x.filter(_(ix) == 1)
      if (2*items_with_ones.length >= x.length) {
        val tmp = x.filter(_(ix) == 0)
        co2Rec(tmp, ix+1)
      } else {
        co2Rec(items_with_ones, ix+1)
      }
    }
  }
  def main_2(): Unit = {
    val src = Source.fromFile("./3-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val lines = linesStr.map(_.toList.map(_.asDigit))
    val oxy = oxyRec(lines, 0)
    val co2 = co2Rec(lines, 0)
    val oxyval = oxy.head.reverse.zipWithIndex.map(binaryConv).sum
    val co2val = co2.head.reverse.zipWithIndex.map(binaryConv).sum
    println(oxyval)
    println(co2val)
    println(oxyval*co2val)
    //println(co2Rec(lines, 0).head.reverse.zipWithIndex.map(binaryConv).sum)

  }
  main_2()
}