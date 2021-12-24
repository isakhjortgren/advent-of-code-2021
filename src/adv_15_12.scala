import scala.io.Source

class OrderNode(node: (Int, Int), cost: Int) {
  def getNode: (Int, Int) = {
    node
  }
  def nodeOrder(): Int = {
    -cost
  }
}

class PathFinder(riskMatrix: List[List[Int]]) {
  val nRows: Int = riskMatrix.length
  val nCols: Int = riskMatrix.head.length
  val optimalAccRisk =  collection.mutable.Map.empty[(Int, Int), Int]
  val lowestRiskNode = collection.mutable.Map.empty[(Int, Int), Int]


  def findLowestRisk(rIx: Int, cIx: Int): Int = {
    if ((rIx == (nRows-1))  && (cIx == (nCols-1))) {
      riskMatrix(rIx)(cIx)
    } else {
      val coords = List((rIx+1, cIx), (rIx, cIx+1), (rIx-1, cIx), (rIx, cIx-1))
      coords.map(x => findLowestRisk(x._1, x._2) + riskMatrix(rIx)(cIx)).min
    }
  }

  def h(node: (Int, Int)): Int = {
    5*(nRows-node._1-1 + nCols-node._2-1)
  }

  def getNode(openSet: collection.mutable.Set[(Int, Int)],
              fScore: collection.mutable.Map[(Int, Int), Int]): (Int, Int) = {
    var minVal = Int.MaxValue
    var bestNode = (-1, -1)
    openSet.foreach(node => {
      if (fScore(node) <= minVal) {
        minVal = fScore(node)
        bestNode = node
      }
    })
    bestNode
  }

  def getNeighbours(node: (Int, Int)): List[(Int, Int)] = {
    val rIx = node._1
    val cIx = node._2
    val coords = List((rIx+1, cIx), (rIx, cIx+1), (rIx-1, cIx), (rIx, cIx-1))
    coords.filter(node => (node._1 >= 0) && (node._2 >= 0) && (node._1 < nRows) && (node._2 < nCols))
  }


  def unwrapScore(node: (Int, Int), costSoFar: Int, cameFrom: collection.mutable.Map[(Int, Int), (Int, Int)]): Int = {
    val prevNode = cameFrom(node)
    if (prevNode == (0,0)) {
      costSoFar + riskMatrix(node._1)(node._2)
    } else {
      val tmpCost = riskMatrix(node._1)(node._2) + costSoFar
      unwrapScore(prevNode, tmpCost, cameFrom)
    }
  }

  def aStar(): Int = {
    val startNode = (0,0)
    val goalNode = (nRows-1, nCols-1)

    //val openSet = collection.mutable.Set(startNode)

    val cameFrom = collection.mutable.Map.empty[(Int, Int), (Int, Int)]

    val gScore = collection.mutable.Map.empty[(Int, Int), Int]
    (0 until nRows).foreach( rIx => (0 until nCols).foreach(cIx => gScore((rIx, cIx)) = Int.MaxValue))
    gScore(startNode) = 0

    val fScore = collection.mutable.Map.empty[(Int, Int), Int]
    (0 until nRows).foreach( rIx => (0 until nCols).foreach(cIx => fScore((rIx, cIx)) = Int.MaxValue))
    fScore(startNode) = h(startNode)

    val priorityQueue: collection.mutable.PriorityQueue[OrderNode] = collection.mutable.PriorityQueue(new OrderNode(startNode, h(startNode)))( Ordering.by(_.nodeOrder()))

    while (priorityQueue.nonEmpty) {
      //val currentNode = getNode(openSet, fScore)
      val currentNode = priorityQueue.dequeue().getNode

      if (currentNode == goalNode) {
        val score = unwrapScore(goalNode, 0, cameFrom)
        println(score)
      }

      getNeighbours(currentNode).foreach{neighbour => {
        val tentativeScore = gScore(currentNode) + riskMatrix(neighbour._1)(neighbour._2)
        if (tentativeScore < gScore(neighbour)) {
          cameFrom(neighbour) = currentNode
          gScore(neighbour) = tentativeScore
          fScore(neighbour) = tentativeScore + h(neighbour)
          //if (!priorityQueue.co) {
          //  openSet.add(neighbour)
          //}
          priorityQueue.addOne(new OrderNode(neighbour, fScore(neighbour)))
        }
      }
      }

    }

    -1
  }
}

object adv_15_12 extends App {

  def loadData(): List[List[Int]] = {
    val src = Source.fromFile("./15-12/input.txt")
    val rawData = src.getLines.toList
    src.close()
    rawData.map(x => x.map(_.asDigit).toList)
  }

  def main1(): Unit = {
    val pathFinder = new PathFinder(loadData())

    //athFinder.findPath(0, 0, 0)
    //println(pathFinder.optimalAccRisk((pathFinder.nRows-1, pathFinder.nCols-1)))
    println(pathFinder.aStar())
  }

  def create2ndMap(riskMatrix: List[List[Int]]): List[List[Int]] = {
    val nRows: Int = riskMatrix.length
    val nCols: Int = riskMatrix.head.length

    val fullRisk = (0 until (5*nRows)).map(rIx => {
      (0 until (5*nCols)).map(cIx => {
        val tmpRIx =  rIx % nRows
        val tmpCIx =  cIx % nCols
        val risk = riskMatrix(tmpRIx)(tmpCIx) + rIx/nRows + cIx/nCols
        if (risk > 9) {
          risk-9
        } else
          risk
      }).toList
    }).toList
    fullRisk
  }
  def main2(): Unit = {
    val oRiskMat = loadData()

    val riskMat = create2ndMap(oRiskMat)

    val pathFinder = new PathFinder(riskMat)
    pathFinder.aStar()

  }

  main2()
}