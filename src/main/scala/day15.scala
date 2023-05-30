import scala.math
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import java.nio.file.Paths

object day15 {

    val sample = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

    case class Pos(x: Int, y: Int)

    def dist(p1: Pos, p2: Pos): Int = {
        math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)
    }

    def parseLine(s: String): (Pos, Pos, Int) = {
        val reg = """-?\d+""".r
        val vs = reg.findAllIn(s).map(_.toInt).toArray
        val a1 = Pos(vs(0), vs(1))
        val a2 = Pos(vs(2), vs(3))
        val d = dist(a1, a2)
        (a1, a2, d)
    }

    def solvePart1(data: Array[(Pos,Pos,Int)], yVal: Int): Int = {

        def computeYRange(y: Int, dist: Int): Range.Inclusive = {
            Range.Inclusive(y - dist, y + dist, 1)
        }

        var rowSet = Set.empty[Int]

        val inRangeY = data.filter{ v => 
            val r = computeYRange(v._1.y, v._3)
            r.contains(yVal)
        }

        val beacons = inRangeY.map(v => v._2).toSet

        inRangeY.foreach{v => 
            val diff = math.abs(v._1.y - yVal)
            val xSmallSpace = v._1.x - (v._3 - diff)
            val xBigSpace = v._1.x + (v._3 - diff)
            //println(s"$v => $xSmallSpace, $xBigSpace")
            for i <- xSmallSpace to xBigSpace do {                
                    rowSet += i
            }                        
        }    

        val beaconsOnY = beacons.filter { v => v.y == yVal }.map { v => v.x }
        rowSet.diff(beaconsOnY).size        
    }

    import scala.util.control.Breaks
    def solvePart2(data: Array[(Pos,Pos,Int)], max: Int = 4_000_000): Long = {
        val len = max + 1
        val rowData = Array.fill(len)(Vector(Range.Inclusive(0, max, 1)))                        
        for (s, b, d) <- data do {
            val top = math.max(0, s.y - d)
            val bot = math.min(max, s.y + d)
            for row <- top to bot do {                
                val dd = math.abs(s.y - row)
                val minX = math.max(0, s.x - (d - dd))
                val maxX = math.min(max, s.x + (d - dd))

                var newRange = Vector.empty[Range.Inclusive]             
                val bk = Breaks()
                for r <- rowData(row) do {
                    bk.breakable{ 
                        val st = r.start
                        if st > maxX then {
                            newRange :+= r
                            bk.break()
                        }

                        val ed = r.`end`
                        if ed < minX then {
                            newRange :+= r
                            bk.break()                        
                        }

                        if st < minX then {
                            newRange :+= Range.Inclusive(st, minX-1, 1)
                        }

                        if ed > maxX then {
                            newRange :+= Range.Inclusive(maxX+1, ed, 1)
                        }
                    }
                }       
                //println(s"ROW:$row => $newRange")         
                rowData(row) = newRange
            }
        }

        val f = rowData.zipWithIndex.find((p,i) => p.nonEmpty)
        if f.isDefined then {
            val y = f.get._2
            val x = f.get._1
            println(s"Y:$y, $x")
            println(s"${x.head.start}")
            val m = x.head.start.toLong * 4000000L + y.toLong            
            m
        } else {
            0L
        }        
    }
}

@main def day15Part1(): Unit = {

    import day15.*

    val data = 
        Files.readAllLines(Paths.get("./inputs/input_day15.txt")).asScala
            .map(parseLine).toArray
    
    // sample data
    //val data = sample.split("\r\n").map(parseLine)
    //println(solvePart1(data, 2000000))

    println(solvePart2(data))
}
