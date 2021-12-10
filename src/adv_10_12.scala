import scala.io.Source

object CorrectCloserException extends Exception { }
object GetScoreException extends Exception { }

object adv_10_12 extends App {

  def isACloser(candidate: Char): Boolean = {
    Set(")", "]", "}", ">").contains(candidate.toString)
  }

  def isCorrectCloser(reference: Char, candidate: Char): Boolean = {
        reference.toString match {
          case "[" => "]" == candidate.toString
          case "(" => ")" == candidate.toString
          case "{" => "}" == candidate.toString
          case "<" => ">" == candidate.toString
          case _ => {
            throw CorrectCloserException
          }
        }
  }

  def getParsedString(line: String): String = {
    line.toList.foldLeft("")((parsedContent, sign) => {
      if (parsedContent.isEmpty) {
        sign.toString
      } else {
        if (isACloser(sign)) {
          parsedContent.take(parsedContent.length-1)
        } else {
          parsedContent + sign
        }
      }
    })
  }

  def getScore(line: String): Int = {
    // for each char
    //      if char is opener add to list
    //      if char is closer check last entry in list
    //            if last in list and closer same type pop last from list
    var lostChars = ""
    line.toList.foldLeft("")((parsedContent, sign) => {
      if (parsedContent.isEmpty) {
        sign.toString
      } else {
        if (isACloser(sign)) {
          if (isCorrectCloser(parsedContent.last, sign)) {
            parsedContent.take(parsedContent.length-1)
          } else {
            lostChars += sign.toString
            parsedContent.take(parsedContent.length-1)
          }

        } else {
          parsedContent + sign
        }
      }
    })
    if (lostChars.isEmpty) 0 else {
      lostChars.head.toString match {
        case "]" => 57
        case ")" => 3
        case "}" => 1197
        case ">" => 25137
        case _ => {
          throw GetScoreException
        }
      }
    }

  }
  def main1(): Unit = {
    val src = Source.fromFile("./10-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val scores = linesStr.map(getScore)
    println(scores.sum)
  }

  def getMissingPart(line: String): String = {
    val parsedString = getParsedString(line)
    "dasda"
  }

  def convertCharToScore(sign: Char): Long = {
    sign match {
      case '(' => 1.toLong
      case '[' => 2.toLong
      case '{' => 3.toLong
      case '<' => 4.toLong
      case _ => throw GetScoreException
    }
  }

  def getScorePart2(leftOver: String): Long = {
    leftOver.toList.reverse.foldLeft(0.toLong)((score, sign) => (score*5 + convertCharToScore(sign)).toLong)
  }

  def getMedian(scores: List[Long]): Long = {
    val medians = scores.grouped(5).map(x => {
      x.sorted.toList(x.length/2)
    }).toList
    if (medians.length == 1) {
      medians.head
    } else if (medians.isEmpty) {
      throw GetScoreException
    } else {
      getMedian(medians)
    }
  }

  def main2(): Unit = {
    val src = Source.fromFile("./10-12/input.txt")
    val linesStr = src.getLines.toList.filter(getScore(_) == 0)
    src.close()
    val incompletes = linesStr.map(getParsedString)
    val scores = incompletes.map(getScorePart2)

    println(getMedian(scores))
    println(scores.sorted.toList(scores.length/2))
  }
  main2()
}
