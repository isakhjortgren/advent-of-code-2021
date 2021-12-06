import scala.io.Source

class Point(val x: Int, val y: Int) {
  def isEqual(other: Point): Boolean = {
    x.equals(other.x) && y.equals(other.y)
  }
  override def toString: String =
    s"($x, $y)"
}

class Line(val p1: Point, val p2: Point)  {
  def isVertical: Boolean = {
    p1.x == p2.x
  }
  def isHorizontal: Boolean = {
    p1.y == p2.y
  }
  def isDiagonal: Boolean = {
    !(isHorizontal || isVertical)
  }
  def isUpperDiagonal: Boolean = {
    ((p1.x > p2.x) && (p1.y > p2.y)) || ((p2.x > p1.x) && (p2.y > p1.y))
  }

  def getPoints: List[Point] = {
    if (this.isVertical) {
      (p1.y.min(p2.y) to p1.y.max(p2.y)).map(new Point(p1.x, _)).toList
    } else if (this.isHorizontal) {
      (p1.x.min(p2.x) to p1.x.max(p2.x)).map(new Point(_, p1.y)).toList
    } else {
      val xVals = p1.x.min(p2.x) to p1.x.max(p2.x)
      val yVals = p1.y.min(p2.y) to p1.y.max(p2.y)
      if (isUpperDiagonal) {
        (xVals zip yVals).map(x => new Point(x._1, x._2)).toList
      } else {
        (xVals zip yVals.reverse).map(x => new Point(x._1, x._2)).toList
      }
    }
  }

  def getIntersectionFromHorizontal(otherVertical: Line): (Boolean, Point) = {
    val xMin = p1.x.min(p2.x)
    val xMax = p1.x.max(p2.x)
    val y = p1.y
    val yMin = otherVertical.p1.y.min(otherVertical.p2.y)
    val yMax = otherVertical.p1.y.max(otherVertical.p2.y)
    val x = otherVertical.p1.x
    if ((xMin <= x && x <= xMax) && (yMin <= y && y <= yMax)) {
      (true, new Point(x, y))
    } else {
      (false, new Point(0, 0))
    }
  }

  def getVerticalIntersection(other: Line): List[(Boolean, Point)] = {
    if (p1.x == other.p1.x) {
      val minThisY = p1.y.min(p2.y)
      val minOtherY = other.p1.y.min(other.p2.y)
      val minMaxY =  minThisY.max(minOtherY)

      val maxThisY = p1.y.max(p2.y)
      val maxOtherY = other.p1.y.max(other.p2.y)
      val maxMinY =  maxThisY.min(maxOtherY)

      if (minMaxY <= maxMinY) {
        val yRange: List[Int] = (minMaxY to maxMinY).toList
        return yRange.map(y => (true, new Point(p1.x, y)))
      }
    }
    List((false, new Point(0, 0)))
  }

  def getHorizontalIntersection(other: Line): List[(Boolean, Point)] = {
    if (p1.y == other.p1.y) {
      val minThisX = p1.x.min(p2.x)
      val minOtherX = other.p1.x.min(other.p2.x)
      val minMaxX =  minThisX.max(minOtherX)

      val maxThisX = p1.x.max(p2.x)
      val maxOtherX = other.p1.x.max(other.p2.x)
      val maxMinX =  maxThisX.min(maxOtherX)

      if (minMaxX <= maxMinX) {
        val xRange: List[Int] = (minMaxX to maxMinX).toList
        val tmp = xRange.map(x => (true, new Point(x, p1.y)))
        return tmp
      }
    }
    List((false, new Point(0, 0)))
  }

  def getDiagonalIntersection(other: Line): List[(Boolean, Point)] = {
    val thisPoints = getPoints.map(p => (p.x, p.y)).toSet
    val otherPointsSet = other.getPoints.map(p => (p.x, p.y)).toSet

    val pointsEqual = thisPoints.intersect(otherPointsSet).map(xy => new Point(xy._1, xy._2)).toList
    if (pointsEqual.length > 0) {
      return pointsEqual.map(p=> (true, p))
    }
    List((false, new Point(0, 0)))
  }

  def getIntersections(other: Line): List[(Boolean, Point)] = {
    if (isHorizontal && other.isVertical) {
      List(getIntersectionFromHorizontal(other))
    } else if (isVertical && other.isHorizontal) {
      List(other.getIntersectionFromHorizontal(this))
    } else if (isVertical && other.isVertical) {
      getVerticalIntersection(other)
    } else if (isHorizontal && other.isHorizontal) {
      getHorizontalIntersection(other)
    } else {
      getDiagonalIntersection(other)
    }

  }

  def getIntersectionsLoop(other: Line): List[(Boolean, Point)] = {
    val ourBoxXMin = p1.x.min(p2.x)
    val ourBoxXMax = p1.x.max(p2.x)
    val ourBoxYMin = p1.y.min(p2.y)
    val ourBoxYMax = p1.y.max(p2.y)
    val otherBoxXMin = other.p1.x.min(other.p2.x)
    val otherBoxXMax = other.p1.x.max(other.p2.x)
    val otherBoxYMin = other.p1.y.min(other.p2.y)
    val otherBoxYMax = other.p1.y.max(other.p2.y)

    val isWithinX = (ourBoxXMin <= otherBoxXMin && otherBoxXMin <= ourBoxXMax) || (ourBoxXMin <= otherBoxXMax && otherBoxXMax <= ourBoxXMax)
    val isWithinY = (ourBoxYMin <= otherBoxYMin && otherBoxYMin <= ourBoxYMax) || (ourBoxYMin <= otherBoxYMax && otherBoxYMax <= ourBoxYMax)

    if (isWithinX && isWithinY) {
      getPoints.flatMap(x1 => other.getPoints.map(x2 => (x1.isEqual(x2), x1)))
    } else {
      List((false, new Point(0, 0)))
    }
  }
}


object adv_5_12 extends App {

  def isVertical(x: List[List[Int]]): Boolean = {
    x.head.head == x(1).head
  }

  def isHorizontal(x: List[List[Int]]): Boolean = {
    x.head(1) == x(1)(1)
  }

  def getIntersection(h: List[List[Int]], v: List[List[Int]]): (Boolean, (Int, Int)) = {
    val h_x1 = h.head.head
    val h_x2 = h(1).head
    val h_y = h.head(1)
    val v_y1 = v.head(1)
    val v_y2 = v(1)(1)
    val v_x = v.head.head
    if (h_y <= v_y1.max(v_y2) && v_y1.min(v_y2) <= h_y) {
      if (v_x <= h_x1.max(h_x2) && h_x1.min(h_x2) <= v_x) {
        return (true, (v_x, h_y))
      }
    }
    (false, (0, 0))
  }
  def getHorizontalIntersection(h1: List[List[Int]], h2: List[List[Int]]): (Boolean, List[(Int, Int)]) = {
    val h1_x1 = h1.head.head
    val h1_x2 = h1(1).head
    val h1_y = h1.head(1)
    val h2_x1 = h2.head.head
    val h2_x2 = h2(1).head
    val h2_y = h2.head(1)

    val h1_xmax = h1_x1.max(h1_x2)
    val h1_xmin = h1_x1.min(h1_x2)
    val h2_xmax = h2_x1.max(h2_x2)
    val h2_xmin = h2_x1.min(h2_x2)

    if (h1_y == h2_y) {
      val maxOfMin = h1_xmin.max(h2_xmin)
      val minOfMax = h1_xmax.min(h2_xmax)
      if (maxOfMin <= minOfMax) {
        val xRange: List[Int] = (maxOfMin to minOfMax).toList
        return (true, xRange.map((_, h1_y)))
      }
    }
    (false, List((0, 0)))
  }

  def getVerticalIntersection(v1: List[List[Int]], v2: List[List[Int]]): (Boolean, List[(Int, Int)]) = {
    val v1_y1 = v1.head(1)
    val v1_y2 = v1(1)(1)
    val v1_x = v1.head.head
    val v2_y1 = v2.head(1)
    val v2_y2 = v2(1)(1)
    val v2_x = v2.head.head

    val v1_ymax = v1_y1.max(v1_y2)
    val v1_ymin = v1_y1.min(v1_y2)
    val v2_ymax = v2_y1.max(v2_y2)
    val v2_ymin = v2_y1.min(v2_y2)

    if (v1_x == v2_x) {
      val maxOfMin = v1_ymin.max(v2_ymin)
      val minOfMax = v1_ymax.min(v2_ymax)
      if (maxOfMin <= minOfMax) {
        val yRange: List[Int] = (maxOfMin to minOfMax).toList
        return (true, yRange.map((v1_x, _)))
      }
    }
    (false, List((0, 0)))
  }

  def main_1() {
    val src = Source.fromFile("./5-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()

    val data: List[List[List[Int]]] = linesStr.map(_.split(" -> ").map(_.split(",").map(_.toInt).toList).toList).toList
    val horizontalLines = data.filter(isHorizontal)
    val verticalLines = data.filter(isVertical)

    val intersections = horizontalLines.flatMap(h => verticalLines.map(v => getIntersection(h, v)))

    val horizontalCombs = horizontalLines.zipWithIndex.flatMap(h1 => horizontalLines.zipWithIndex.map(h2 => {
      (h1._2 == h2._2, (h1._1, h2._1))
    })).filter(!_._1).map(_._2)

    val verticalCombs = verticalLines.zipWithIndex.flatMap(l1 => verticalLines.zipWithIndex.map(l2 => {
      (l1._2 == l2._2, (l1._1, l2._1))
    })).filter(!_._1).map(_._2)

    val horizontalIntersections = horizontalCombs.map(x => getHorizontalIntersection(x._1, x._2)).filter(_._1).flatMap(_._2)

    val verticalIntersections = verticalCombs.map(x => getVerticalIntersection(x._1, x._2)).filter(_._1).flatMap(_._2)

    val coords = intersections.filter(_._1).map(_._2)
    val allInts = coords ::: horizontalIntersections ::: verticalIntersections
    val nIntersections = allInts.groupBy( v => (v._1,v._2)).keys.toList.length
    println(nIntersections)
    println(7380)


  }

  def mainBetter(): Unit = {
    val src = Source.fromFile("./5-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()

    val data = linesStr.map(_.split(" -> ").map(_.split(",").map(_.toInt).toList).toList)
    val lines = data.map(line => {
      val p1 = new Point(line.head.head, line.head(1))
      val p2 = new Point(line(1).head, line(1)(1))
      new Line(p1, p2)
    })
    val horiAndVerti = lines.filter(l => l.isHorizontal || l.isVertical)
    val potentialIntersections = horiAndVerti.zipWithIndex.flatMap(x => {
      horiAndVerti.drop(x._2+1).flatMap(line => line.getIntersections(x._1))
    })

    //val tmpInters = horiAndVerti(1).getIntersections(horiAndVerti(4))

    val intersections = potentialIntersections.filter(_._1).map(_._2)
    val uniqueIntersections = intersections.groupBy(p => (p.x, p.y)).keys.toList

    println(uniqueIntersections)
    println(uniqueIntersections.length)
    println(7380)
  }
  def main2(): Unit ={
    val src = Source.fromFile("./5-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()

    val data = linesStr.map(_.split(" -> ").map(_.split(",").map(_.toInt).toList).toList)
    val lines = data.map(line => {
      val p1 = new Point(line.head.head, line.head(1))
      val p2 = new Point(line(1).head, line(1)(1))
      new Line(p1, p2)
    })

    val potentialIntersections = lines.zipWithIndex.flatMap(x => {
      lines.drop(x._2+1).flatMap(line => line.getIntersections(x._1))
    })

    //val tmpInters = lines(6).getIntersections(lines(9))

    val intersections = potentialIntersections.filter(_._1).map(_._2)
    val uniqueIntersections = intersections.groupBy(p => (p.x, p.y)).keys.toList

    //println(uniqueIntersections)
    println(uniqueIntersections.length)
    println(7380)
  }
  main2()
}