import java.nio.file.Path
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import java.nio.file.Paths
object day12 {

    val SChar = 83
    val EChar = 69

    val startChar = ('a' - 1).toChar
    val endChar = ('z'+1).toChar

    case class Pos(x: Int, y: Int)
    case class PosAndDist(pos: Pos, dist: Int)

    var startPos: Pos = Pos(-1,-1)
    var endPos: Pos = Pos(-1,-1)

    /**
      * search S and E also replace other number
      * @param path
      * @return
      */
    def createGridFromFile(path: Path): Array[Array[Char]] = {
        val res = 
            val buf = Files.readAllLines(path).asScala
            for 
                i <- 0 until buf.length                
            yield {
                val arr = buf(i).toCharArray()
                for j <- 0 until arr.length do {
                    if arr(j) == 'S' then {
                        arr(j) = startChar
                        startPos = Pos(j, i)
                    } else if arr(j) == 'E' then {
                        arr(j) = endChar
                        endPos = Pos(j, i)
                    }
                }
                arr
            }
        res.toArray
    }

    def printGrid(grid: Array[Array[Char]]): Unit = {
        for y <- grid do {
            println(y.mkString(""))
        }            
    }

    // part2 E from search a short path
    import scala.util.control.Breaks

    def findPossibleNexts(p: Pos, pdist: Int, shortGrid: Array[Array[Int]], arr: Array[Array[Char]] ): Vector[Pos] = {
        var res = Vector.empty[Pos]
        val pc = arr(p.y)(p.x)
        for 
            p2 <- List(Pos(-1,0), Pos(1,0), Pos(0,1), Pos(0,-1))
            n = Pos(p.x + p2.x, p.y + p2.y)
            if n.x >= 0 && n.x < shortGrid(0).length && n.y >= 0 && n.y < shortGrid.length            
            if pc - arr(n.y)(n.x) < 2
            if shortGrid(n.y)(n.x) > pdist + 1
        do {
            res :+= n
        }
        res
    }

    def searchPart2Path(grid: Array[Array[Char]]): Int = {
        given Ordering[PosAndDist] = Ordering.by(s => -1 * s.dist)
        val pq = scala.collection.mutable.PriorityQueue.empty[PosAndDist]
        val shortGrid = Array.fill(grid.length, grid(0).length)(999_999_999)
        shortGrid(endPos.y)(endPos.x) = 0
        pq.enqueue(PosAndDist(endPos, 0))
        val outBreak = Breaks()
        var res = 0
        outBreak.breakable {
            while pq.nonEmpty do {
                val PosAndDist(cur, curDist) = pq.dequeue()
                if grid(cur.y)(cur.x) == 'a' || grid(cur.y)(cur.x) == startChar then {
                    res = curDist
                    outBreak.break()
                }
                val nx = findPossibleNexts(cur, curDist, shortGrid, grid)
                for n <- nx do {
                    shortGrid(n.y)(n.x) = curDist + 1
                    pq.enqueue(PosAndDist(n, curDist+1))
                }
            }
        }        
        res
    }

}

@main def day12Part2: Unit = {
    import day12.*
    val grid = createGridFromFile(Paths.get("./inputs/input_day12.txt"))
    println(searchPart2Path(grid))
}