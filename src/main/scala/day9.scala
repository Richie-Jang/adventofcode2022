import java.nio.file.{Files, Paths}

object day9 {
    val sample = """R 4
                   |U 4
                   |L 3
                   |D 1
                   |R 4
                   |D 1
                   |L 5
                   |R 2""".stripMargin

    val sample2 = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"""

    // x -> y
    // part1
    var hPos = 0 -> 0
    // part1
    var tPos = 0 -> 0
    // part2
    var tPoss = Array.fill(10)(0 -> 0)
    var tPosMap = Set(tPos)

    enum Dir {
        case L,R,U,D
    }

    case class Motion(dir: Dir, count: Int)

    def parseLine(s: String): Motion  = {
        val s1 = s.split(' ')
        val dir =
        s1(0) match {
            case "R" => Dir.R
            case "L" => Dir.L
            case "U" => Dir.U
            case _ => Dir.D
        }
        Motion(dir, s1(1).toInt)
    }

    import scala.math
    def updateTailPos(): Unit = {
        val dx = math.abs(hPos._1 - tPos._1)
        val dy = math.abs(hPos._2 - tPos._2)
        (dx,dy) match {
            case (a,_) if a >= 2 && hPos._1 > tPos._1 => tPos = hPos.copy(_1 = hPos._1 - 1)
            case (a,_) if a >= 2 => tPos = hPos.copy(_1 = hPos._1 + 1)
            case (_,a) if a >= 2 && hPos._2 > tPos._2 => tPos = hPos.copy(_2 = hPos._2 - 1)
            case (_,a) if a >= 2 => tPos = hPos.copy(_2 = hPos._2 + 1)
            case (_,_) => ()
        }
        tPosMap += tPos
    }

    /**
      * for part1      
      * @param m
      */
    def handleMotion(m: Motion): Unit = {
        m.dir match {
            case Dir.D => hPos = hPos.copy(_2 = hPos._2 + 1)
            case Dir.L => hPos = hPos.copy(_1 = hPos._1 - 1)
            case Dir.R => hPos = hPos.copy(_1 = hPos._1 + 1)
            case _ => hPos = hPos.copy(_2 = hPos._2 - 1)  // up
        }
        updateTailPos()
        //println(s" hPos $tPos")
    }

    /*** Part2 ***/
    
    /**
     * part2 
     */
    def updateTailPos(hIdx: Int, tIdx: Int): Unit = {                
        val hpos = tPoss(hIdx)
        val tpos = tPoss(tIdx)
        if hpos == tpos then return
        val dx = math.abs(hpos._1 - tpos._1)
        val dy = math.abs(hpos._2 - tpos._2)
        (dx,dy) match {
            case (a, b) if a >= 2 && b >= 2 && hpos._1 > tpos._1 =>
                if hpos._2 > tpos._2 then tPoss(tIdx) = hpos._1-1 -> (hpos._2-1)
                else tPoss(tIdx) = hpos._1-1 -> (hpos._2+1)

            case (a, b) if a >= 2 && b >= 2 && hpos._1 < tpos._1 =>
                if hpos._2 > tpos._2 then tPoss(tIdx) = hpos._1+1 -> (hpos._2-1)
                else tPoss(tIdx) = hpos._1+1 -> (hpos._2+1)

            case (a,_) if a >= 2 && hpos._1 > tpos._1 => tPoss(tIdx) = hpos.copy(_1 = hpos._1 - 1)
            case (a,_) if a >= 2 => tPoss(tIdx) = hpos.copy(_1 = hpos._1 + 1)
            case (_,a) if a >= 2 && hpos._2 > tpos._2 => tPoss(tIdx) = hpos.copy(_2 = hpos._2 - 1)
            case (_,a) if a >= 2 => tPoss(tIdx) = hpos.copy(_2 = hpos._2 + 1)
            case (_,_) => ()
        }        
        if tIdx == 9 then tPosMap += tPoss(tIdx)
        else updateTailPos(tIdx, tIdx+1)
    }

    /**
      * for part2      
      * @param m
      * @param head
      * @param index
      */
    def handleMotion(m: Motion, headIdx: Int): Unit = {
        val hPos = tPoss(headIdx)        
        m.dir match {
            case Dir.D => tPoss(headIdx) = hPos.copy(_2 = hPos._2 + 1)
            case Dir.L => tPoss(headIdx) = hPos.copy(_1 = hPos._1 - 1)
            case Dir.R => tPoss(headIdx) = hPos.copy(_1 = hPos._1 + 1)
            case _ => tPoss(headIdx) = hPos.copy(_2 = hPos._2 - 1)  // up
        }
        updateTailPos(headIdx, 1)
        println(s"${tPoss.mkString(" ")}")
    }
}

@main def part1Day9(): Unit = {
    import day9.*
    //val motions = sample.split("\n").map(a => parseLine(a.trim))
    val motions = {
        val f = Files.readString(Paths.get("./inputs/input_day9.txt"))
        f.split("\n").map(a => parseLine(a.trim))
    }
    for m <- motions do {
        for i <- 1 to m.count do {
            handleMotion(m)
        }
    }
    println(tPosMap.size)
}

@main def part2Day9: Unit = {
    import day9.*

    //val motions = sample2.split("\n").map(a => parseLine(a.trim))
    val motions = {
        val f = Files.readString(Paths.get("./inputs/input_day9.txt"))
        f.split("\n").map(a => parseLine(a.trim))
    }
        
    for m <- motions do {        
        for i <- 1 to m.count do {                                 
            handleMotion(m, 0)
        }
    }

    println(tPosMap.size)
}