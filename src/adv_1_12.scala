import scala.io.Source

object adv_1_12 extends App {

  def main_1(): Unit = {
    val linesStr = Source.fromFile("./1-12/input.txt").getLines.toList
    val lines = linesStr.map(x => x.toInt)

    val zipped: List[(Int, Int)] = lines.drop(1) zip lines.dropRight(1)

    val diff = zipped.map{x => x._1 - x._2}

    val largerThan0 = diff.filter(_>0)
    println(largerThan0.size)
  }

  def main_1_2(): Unit = {
    val linesStr = Source.fromFile("./1-12/input.txt").getLines.toList
    val lines = linesStr.map(x => x.toInt)
    val diff = lines.drop(1).zipWithIndex.map(x => x._1 - lines(x._2))
    println(diff.count(x => x>0))
  }

  def main_2_2(): Unit = {
    val linesStr = Source.fromFile("./1-12/input.txt").getLines.toList
    val lines = linesStr.map(x => x.toInt)
    val sumwindow = lines.drop(2).zipWithIndex.map(x => x._1 + lines(x._2) + lines(x._2 + 1))
    val diff = sumwindow.drop(1).zipWithIndex.map(x => x._1 - sumwindow(x._2))
    println(diff.count(x => x>0))
  }

  def main_2(): Unit = {
    val linesStr = Source.fromFile("./1-12/input.txt").getLines.toList
    val lines = linesStr.map(x => x.toInt)

    val zipped: List[(Int, Int)] = lines.drop(2) zip lines.dropRight(1).drop(1)
    val sum_1_2 = zipped.map{x => x._1 + x._2}
    val zipped2: List[(Int, Int)] = sum_1_2 zip lines.dropRight(2)

    val sum_all = zipped2.map{x => x._1 + x._2}

    val zipped_diff: List[(Int, Int)] = sum_all.drop(1) zip sum_all.dropRight(1)
    val diff = zipped_diff.map{x => x._1 - x._2}

    val largerThan0 = diff.filter(_>0)
    println(largerThan0.size)
  }
  main_2()
  main_2_2()
}