import scala.io.Source
object adv_6_12 extends App {

  def oneGeneration(timers: List[Int]): List[Int] = {
    val newTimers = timers.map(_-1)
    val newBorns: Int = newTimers.count(_<0)
    val wrappedTimers = newTimers.map(x => if (x < 0) 6 else x)
    val newBornTimers = List.fill(newBorns)(8)
    wrappedTimers ::: newBornTimers
  }



  def main1() {
    val src = Source.fromFile("./6-12/input.txt")
    val timers = src.getLines.toList.head.split(",").map(_.toInt).toList
    src.close()
    var stat = timers
    for (day <- 1 to 80) {
      stat = oneGeneration(stat)
      //println(s"Day $day: Length ${stat.length}, result: $stat")
    }
    println(s"nLaterns: ${stat.length}")
  }

  def initCollection(timers: List[Long]): Map[Long, Long] = {
    timers.groupBy(x=>x) map {case (key, value) => (key, value.length)}
  }

  def oneSmartGeneration(collection: Map[Long, Long]): Map[Long, Long] = {
    var x: Map[Long, Long] = Map()
    val nRespawners: Long = collection.getOrElse(0, 0)
    for (ix <- 0 to 8) {
      if (ix == 0) {
        x = x.updated(8, nRespawners)
      } else if (ix == 7) {
        val n6s: Long = nRespawners + collection.getOrElse(7, 0:Long)
        x = x.updated(6, n6s)
      } else {
        x = x.updated(ix-1, collection.getOrElse(ix, 0))
      }
    }
    x

  }
  def main2() {
    val src = Source.fromFile("./6-12/input.txt")
    val timers = src.getLines.toList.head.split(",").map(_.toLong).toList
    src.close()

    var coll = initCollection(timers)
    //val fgen = oneSmartGeneration(coll)
    for (day <- 1 to 256) {
      coll = oneSmartGeneration(coll)
      //println(s"Day $day: nfishes ${coll.values.sum}")
    }
    println(s"nfishes ${coll.values.sum}")
  }
  main2()
}
