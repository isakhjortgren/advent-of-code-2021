import scala.io.Source

class Instructions(instructionsLookupReplacement: Map[String, String], instructionsLookupImpute: Map[String, String]) {

  def recursiveCount(seqCount: Map[String, Long]): Map[String, Long] = {

    val tmp = seqCount.keys.toList.flatMap(k => instructionsLookupReplacement(k).sliding(2).toList.map(x=> (x, seqCount.getOrElse(k, 0.toLong))))
    val res0 = tmp.groupBy(x => x._1)
    val res = res0.transform((_,y) => y.map(x => x._2).sum)
    res
  }
}

object adv_14_12 extends App {

  def imputeChar(comb: String, instructions: List[(String, String)]): String = {
    val instruction = instructions.filter(_._1 == comb)
    if (instruction.isEmpty) {
      comb
    } else {
      instruction.head._2 + comb(1).toString
    }
  }
  def oneIteration(sequence: String, instructions: List[(String, String)]): String = {
    val temp = sequence.sliding(2).map(x => imputeChar(x, instructions))
    sequence(0).toString + temp.reduce(_ + _)
  }

  def main1(): Unit = {
    val src = Source.fromFile("./14-12/input.txt")
    val rawData = src.getLines.toList
    src.close()
    val sequence = rawData.head
    val instructions = rawData.drop(2).map(x => (x.take(2), x.drop(6)))

    val finalSequence = (1 to 10).foldLeft(sequence)((itSeq, _)=> oneIteration(itSeq, instructions))
    val charCounts = finalSequence.toList.groupBy(x => x).values.map(_.length)
    println(charCounts.max - charCounts.min)
  }

  def main2(): Unit = {
    val src = Source.fromFile("./14-12/input.txt")
    val rawData = src.getLines.toList
    src.close()
    val sequence = rawData.head
    val instructions = rawData.drop(2).map(x => (x.take(2), x.drop(6)))
    val instructionsLookupReplacement =  instructions.map(x => {
      x._1 -> (x._1(0) + x._2 + x._1(1).toString)
    }).toMap
    val instructionsLookupImpute = instructions.map(x => x._1 -> x._2).toMap
    val inst = new Instructions(instructionsLookupReplacement, instructionsLookupImpute)
    val seqCount = sequence.sliding(2).toList.groupBy(x=>x).transform((x, y) => y.length.toLong)
    val seq2 = (1 to 40).foldLeft(seqCount)((itSeq, _) => inst.recursiveCount(itSeq))
    val res = seq2.groupBy(kv => kv._1(1)).transform((ch, mp) => {
      if (ch == sequence(0)) {
        mp.values.sum + 1
      } else {
        mp.values.sum
      }

    })

    println(res.values.max - res.values.min)




  }

  main2()

}
