/**
  * Imagine a matrix of 3 rows and columns
  * The idea is to find the path to start at top left corner and go to bottom right corner
  */
class PathDetection {
  type Position = (Int, Int)
  val xSize = 3
  val ySize = 3
  val inputMatrix:List[Int] = List(4, 5, 6, 1, 2, 3, 0, 0, 1)
  def getValueOfPos(pos:Position):Int = {
    inputMatrix(pos._2 * xSize + pos._1)
  }
  trait Move {
    def execute(curPos:Position): Position
  }
  object Right extends Move {
    def execute(curPos:Position) = {
      (curPos._1 + 1, curPos._2)
    }
  }
  object Down extends Move {
    def execute(curPos:Position) = {
      (curPos._1, curPos._2 + 1)
    }
  }
  object Diagonal extends Move {
    def execute(curPos:Position) = {
      (curPos._1 + 1, curPos._2 + 1)
    }
  }
  def getAllValidMoves(curPos:Position):List[Move] = {
    var rtnMoves:List[Move] = List()
    if(curPos._1 < xSize-1) rtnMoves = Right :: rtnMoves
    if(curPos._2 < ySize-1) rtnMoves = Down :: rtnMoves
    if(curPos._1 < xSize-1 && curPos._2 < ySize-1) rtnMoves = Diagonal :: rtnMoves
    rtnMoves
  }
  def distance(pos1:Position, pos2:Position):Int = {
    math.max(math.abs(pos1._1 - pos2._1), math.abs(pos1._2 - pos2._2))
  }
  class Path(val history:List[Move], val endPos:Position, val sumCost:Int) {
    def extend(m:Move):Path = new Path(m :: history, m.execute(endPos), sumCost + getValueOfPos(m.execute(endPos)))
    def a9Heuristic(goalPos:Position):Int = sumCost + distance(endPos, goalPos)
  }
  val initialPath:Path = new Path(List(), (0,0), 4)
  def bruteForceFindAllPaths(lip:List[Path]):List[Path] = {
    if(lip.length == 0) Nil
    else {
      val more = for {
                    p <- lip
                    next <- getAllValidMoves(p.endPos) map p.extend
                  } yield next
      lip ::: bruteForceFindAllPaths(more)
    }
  }
  def findMinCost(path1:Path, path2:Path): Path = {
    if(path1.sumCost <= path2.sumCost) path1 else path2
  }
  val solution = bruteForceFindAllPaths(List(initialPath)).filter(_.endPos == (2,2)).reduceRight(findMinCost)

  import collection.immutable.SortedSet
  val orderByPathLength = Ordering[(Int, String)].on[Path](s => s.history.length -> s.history.toString())
  val orderByCost = Ordering[(Int, String)].on[Path](s => s.sumCost -> s.history.toString())
  val orderByCostAndDistToGoal = Ordering[(Int, String)].on[Path](s => s.a9Heuristic((2,2)) -> s.history.toString())

  val bfsSet = SortedSet(initialPath)(orderByPathLength)
  val uniformCostSet = SortedSet(initialPath)(orderByCost)
  val a9Set = SortedSet(initialPath)(orderByCostAndDistToGoal)

  def pathSearcher(setOfPaths: SortedSet[Path], goalPosition: Position):SortedSet[Path] = {
    if (setOfPaths.isEmpty) SortedSet(initialPath)(orderByCost)
    else {
      if(setOfPaths.head.endPos == goalPosition) return setOfPaths
      val more:List[Path] = getAllValidMoves(setOfPaths.head.endPos).map(setOfPaths.head.extend)
      pathSearcher(setOfPaths.tail ++ more, goalPosition)
    }
  }
}

val p = new PathDetection

val pa = p.initialPath
pa.endPos
pa.extend(p.Right).sumCost
p.solution.history.reverse

p.pathSearcher(p.bfsSet, (2,2)).head.history.reverse
p.pathSearcher(p.uniformCostSet, (2,2)).head.history.reverse
p.pathSearcher(p.a9Set, (2,2)).head.history.reverse








