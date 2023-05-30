import java.nio.file.{Files, Paths}
import scala.util.chaining.*

object day14 {

    val sample = """498,4 -> 498,6 -> 496,6
                 |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

    case class Pos(x: Int, y: Int)
    // init no meaning
    var startPos: Pos = Pos(-1, -1)
    // part2
    // max + 2
    var abyssPosY: Int = -1

    /** parseInput
      * @param inp
      * @return
      *   Pos 2dArr, minPos, maxPos
      */
    private def parseInput(inp: String): (Array[Array[Pos]], Pos, Pos) = {
        var minX, minY = 1000
        var maxX, maxY = 0
        val posArr = inp.split("\r\n").map { s =>
            val ss = s.split(" -> ")
            ss.map { v =>
                val vv = v.split(',')
                val r = Pos(vv(0).toInt, vv(1).toInt)
                if minX > r.x then minX = r.x
                if minY > r.y then minY = r.y
                if maxX < r.x then maxX = r.x
                if maxY < r.y then maxY = r.y
                r
            }
        }
        (posArr, Pos(minX, minY), Pos(maxX, maxY))
    }

    def createGrid(inp: String): Array[Array[Char]] = {
        val (data, min, max) = parseInput(inp)
        println(s"Min: $min")
        println(s"Max: $max")
        // compensation x left/right
        val compX = 300
        abyssPosY = max.y + 2

        // add left / right 10 more space
        val width = max.x - min.x + (compX * 2) + 1
        // include abyss
        val height = max.y + 3

        println(s"Width: $width, Height: $height")

        val res = Array.fill(height, width)('.')

        // fill abyss
        for x <- 0 until width do {
            res(abyssPosY)(x) = '#'
        }

        def makeAToB(a: Pos, b: Pos): ((Int, Int), (Int, Int)) = {
            val x = if a.x < b.x then a.x -> b.x else b.x -> a.x
            val y = if a.y < b.y then a.y -> b.y else b.y -> a.y
            x -> y
        }

        // width control : v - min.x + 1 { because left / right one space more }
        for l <- data do {
            var prePos: Pos = l(0).pipe(a => Pos(a.x - min.x + compX, a.y))
            for pos <- l do {
                val pos1 = pos.pipe(a => Pos(a.x - min.x + compX, a.y))
                val (xd, yd) = makeAToB(pos1, prePos)
                if xd._2 - xd._1 != 0 then {
                    for x <- xd._1 to xd._2 do {
                        res(yd._1)(x) = '#'
                    }
                } else if yd._2 - yd._1 != 0 then {
                    for y <- yd._1 to yd._2 do {
                        res(y)(xd._1) = '#'
                    }
                } else {
                    // same
                    res(yd._1)(xd._1) = '#'
                }
                prePos = pos1
            }
        }

        // update startPos
        startPos = Pos(500 - min.x + compX, 0)
        // res(startPos.y)(startPos.x) = '+'

        res
    }

    def printGrid(grid: Array[Array[Char]]): Unit = {
        for g <- grid
        do {
            println(g.mkString(""))
        }
    }

    // part1
    import scala.util.control.NonLocalReturns.*
    def runCycle(count: Int, grid: Array[Array[Char]]): Unit = returning {

        def updateSandPos(sp: Pos): Unit = {
            // part1
            if sp.y >= abyssPosY then
                throwReturn[Unit] {
                    println(s"RES: $count")
                }

            // check next pos
            val (nx, ny) = sp.x -> (sp.y + 1)
            grid(ny)(nx) match {
                case '.'       => updateSandPos(Pos(nx, ny))
                case '#' | 'o' =>
                    // check left
                    if grid(ny)(nx - 1) == '.' then
                        updateSandPos(Pos(nx - 1, ny))
                    else if grid(ny)(nx + 1) == '.' then
                        updateSandPos(Pos(nx + 1, ny))
                    else grid(sp.y)(sp.x) = 'o'
                case _ => ()
            }
        }
        updateSandPos(startPos)
        println(s"Cycle: $count === ")
        printGrid(grid)
        runCycle(count + 1, grid)
    }

    import scala.util.control.Breaks
    def part2Cycle(grid: Array[Array[Char]]): Int = {
        val bk = Breaks()
        var res = 0

        println(s"StartPos: $startPos")

        def updateSandPos(sp: Pos): Unit = {
            // part2
            if grid(startPos.y)(startPos.x) == 'o' then {
                bk.break()
            }
            // check next pos
            val (nx, ny) = sp.x -> (sp.y + 1)
            grid(ny)(nx) match {
                case '.'       => updateSandPos(Pos(nx, ny))
                case '#' | 'o' =>
                    // check left
                    if grid(ny)(nx - 1) == '.' then
                        updateSandPos(Pos(nx - 1, ny))
                    else if grid(ny)(nx + 1) == '.' then
                        updateSandPos(Pos(nx + 1, ny))
                    else grid(sp.y)(sp.x) = 'o'
                case _ => ()
            }
        }

        bk.breakable {
            while true do {
                updateSandPos(startPos)
                res += 1
                println(s"Count: $res")
                // if res >= 2944 then printGrid(grid)
            }
        }
        res
    }

}

@main def day14Part1(): Unit = {
    import day14.*

    val grid = createGrid(
      Files.readString(Paths.get("./inputs/input_day14.txt"))
    )
    // val grid = createGrid(sample)
    printGrid(grid)

    runCycle(0, grid)

    printGrid(grid)
}

@main def day14Part2(): Unit = {
    import day14.*
    val grid = createGrid(
      Files.readString(Paths.get("./inputs/input_day14.txt"))
    )
    // val grid = createGrid(sample)
    // printGrid(grid)

    // result
    println(part2Cycle(grid))
    printGrid(grid)

}
