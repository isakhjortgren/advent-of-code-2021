import scala.io.Source
object adv_8_12 extends App {
  def main1(): Unit = {
    val src = Source.fromFile("./8-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()

    val output = linesStr.map(x => x.split("\\s\\|\\s")(1)).flatMap(x => x.split(' ').map(_.length))
    println(output.count(x => !(x == 5 || x == 6)))//output.count(x => !(x.length == 5 || x.length == 6)))

  }
  def decodeStuff(sigPattern: List[String], outputValues: List[String]): Int = {
    val setFor1 = sigPattern.filter(_.length==2).head.toSet
    val setFor4 = sigPattern.filter(_.length==4).head.toSet
    val numbers: List[Int] = outputValues.map(x => {
      x.length match {
        case 2 => 1
        case 3 => 7
        case 4 => 4
        case 7 => 8
        case 5 => { // 2,3,5
          if (5 == x.toSet.union(setFor1).toList.length) {
            3 // 3 contains the numbers from 1
          } else if (x.toSet.union(setFor4).toList.length == 7) {
            2 // 2 union 4 = 8
          } else {
            5
          }
        }
        case 6 => {
          if (x.toSet.union(setFor1).toList.length == 7) {
            6 // 6 union 1 = 8
          } else if (x.toSet.union(setFor4).toList.length == 6) {
            9
          } else {
            0
          }
        }
        case _ => -700000000
      }
    })
    numbers.reverse.zipWithIndex.map(x => x._1*math.pow(10, x._2)).sum.toInt
  }

  def main2(): Unit = {
    val src = Source.fromFile("./8-12/input.txt")
    val linesStr = src.getLines.toList.map(x => x.split("\\s\\|\\s").map(_.split(" "))).map(x => (x(0).toList, x(1).toList))
    src.close()
    val tmp = linesStr.map(x => decodeStuff(x._1, x._2))

    println(tmp.sum)
  }
  main2()
}