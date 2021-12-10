import scala.io.Source



object adv_9_12 extends App {

  def isLowPoint(heightMap: List[List[Int]], rowIx: Int, colIx: Int): Boolean = {
    val nRows = heightMap.length
    val nCols = heightMap.head.length
    val currentValue = heightMap(rowIx)(colIx)
    val cUpper = if (rowIx > 0) heightMap(rowIx-1)(colIx) > currentValue else true
    val cleft = if (colIx > 0) heightMap(rowIx)(colIx-1) > currentValue else true
    val cDown = if (rowIx < (nRows-1)) heightMap(rowIx+1)(colIx) > currentValue else true
    val cRight = if (colIx < (nCols-1)) heightMap(rowIx)(colIx+1) > currentValue else true
    cUpper && cleft && cDown && cRight
  }
  def main1(): Unit = {
    val src = Source.fromFile("./9-12/input2.txt")
    val linesStr = src.getLines.toList
    src.close()
    val heightMap: List[List[Int]] = linesStr.map(line => line.map(_.asDigit).toList)


    val res = heightMap.zipWithIndex.flatMap(row => {
      row._1.zipWithIndex.filter(col => {
        isLowPoint(heightMap, row._2, col._2)
      }).map(col => (col._1, row._2, col._2))
    })
    println(res.map(x => x._1 + 1).sum)
  }

  def findBasinCoords(heightMap: List[List[Int]], rowIx: Int, colIx: Int, alreradyCheckedNodes: Set[(Int, Int)]): Set[(Int, Int)] = {
    val currentValue = heightMap(rowIx)(colIx)
    val nRows = heightMap.length
    val nCols = heightMap.head.length
    val coordCandidates = List((rowIx-1, colIx), (rowIx, colIx-1), (rowIx+1, colIx), (rowIx, colIx+1))
    val coords = coordCandidates.filter(coord => {
      val c1 = (0 <= coord._1) && (coord._1 < nRows)
      val c2 = (0 <= coord._2) && (coord._2 < nCols)
      c1 && c2 && (currentValue < heightMap(coord._1)(coord._2)) && (heightMap(coord._1)(coord._2) != 9)
    }).filter(!alreradyCheckedNodes.contains(_))
    if (coords.isEmpty) {
      alreradyCheckedNodes
    } else {
      val foldres = coords.foldLeft(alreradyCheckedNodes)((x,y) => {
        val tmp = x.union(findBasinCoords(heightMap, y._1, y._2, x)).union(Set(y))
        tmp
      })
      foldres
    }



  }

  def main2(): Unit = {
    val src = Source.fromFile("./9-12/input.txt")
    val linesStr = src.getLines.toList
    src.close()
    val heightMap: List[List[Int]] = linesStr.map(line => line.map(_.asDigit).toList)

    val lowPoints = heightMap.zipWithIndex.flatMap(row => {
      row._1.zipWithIndex.filter(col => {
        isLowPoint(heightMap, row._2, col._2)
      }).map(col => (row._2, col._2))
    })

    val basinSizes = lowPoints.map(coord => findBasinCoords(heightMap, coord._1, coord._2, Set(coord)).size)
    println(basinSizes.sorted.reverse.take(3).product)

  }

  main2()
}