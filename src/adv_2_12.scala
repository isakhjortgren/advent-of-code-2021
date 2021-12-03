import scala.io.Source

object adv_2_12 extends App {

  def main_1(): Unit = {
    val linesStr = Source.fromFile("/Users/isakhjortgren/Documents/advent-of-code-2021/2-12/input.txt").getLines.toList
    val linesTuple: List[(String, Int)] = linesStr.map(x => x.split(" ")).map(x => (x(0), x(1).toInt))
    val length = linesTuple.filter(_._1 == "forward").map(_._2).sum
    val depth = linesTuple.filter(_._1 == "down").map(_._2).sum - linesTuple.filter(_._1 == "up").map(_._2).sum
    println(length*depth)

  }

  def main_2(): Unit = {
    val src = Source.fromFile("/Users/isakhjortgren/Documents/advent-of-code-2021/2-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val linesTuple: List[(String, Int)] = linesStr.map(x => x.split(" ")).map(x => (x(0), x(1).toInt))
    var depth: Int = 0
    var aim: Int = 0
    val length: Int = linesTuple.filter(_._1 == "forward").map(_._2).sum
    linesTuple.foreach(x => {
      x._1 match {
        case "forward" => depth += aim*x._2
        case "down" => aim += x._2
        case "up" => aim -= x._2
      }
    })
    println(length*depth)
  }
  main_2()
}