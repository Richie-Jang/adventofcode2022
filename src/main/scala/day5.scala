import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.FileReader
import java.io.StringReader
import scala.collection.mutable.Stack


@main def day5: Unit = {

    case class Move(count: Int, fromPos: Int, toPos: Int)

    val br = new BufferedReader(new FileReader("./inputs/input_day5.txt"))

    /* val br = new BufferedReader(new StringReader(""" 1   2   3
    [D]    
[N] [C]    
[Z] [M] [P]

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2""")) */

    var arrCount = 0
    var arrStack = Array.empty[Stack[Char]]
    var movements = Vector.empty[Move]
    val moveReg = """move (\d+) from (\d+) to (\d+)""".r.anchored

    def readStackLine(s: String): Unit = {
        val len = s.length()
        var searchers = Vector.empty[Int]
        for i <- 0 until len do {
            val v = i % 4
            if v == 1 then {
                searchers :+= i
            }
        }
        for search <- searchers do {
            val v = search / 4
            if s(search) != ' ' then {
                arrStack(v).push(s(search))
            }
        }
    }

    def parseMoveLine(s: String): Unit = {
        s match {
            case moveReg(a1,b1,c1) => movements :+= Move(a1.toInt, b1.toInt-1, c1.toInt-1)
            case _ => throw Exception(s"$s can not parse")
        }
    }

    def readLines(): Unit = {
        val l = br.readLine()
        if l == null then return            
        if l == "" then {
            // reverse
            for i <- 0 until arrStack.length do {
                arrStack(i) = arrStack(i).reverse
            }
            println("Stack Reversed: "+arrStack.mkString(" "))            
        } else {
            l match {        
            case a if a.startsWith("move") => parseMoveLine(l)
            // get arrCount
            case a if !a.contains("[") => {
                val ll = a.trim.split("\\s+")
                println(ll.mkString(" : "))
                arrCount = ll(ll.length-1).toInt
                arrStack = Array.fill(arrCount)(Stack.empty)
                println(s"ArrayCount : $arrCount")
            }                    
            case a => readStackLine(l)            
            }
        }        
        readLines()
    }

    readLines()

    br.close()
    // movements

    // part1

    /* for m <- movements do {
        for c <- 1 to m.count do {
            val p = arrStack(m.fromPos).pop()
            arrStack(m.toPos).push(p)            
        }
        println("Debug: "+arrStack.mkString(" "))
    }*/
    
    // part2
    for m <- movements do {
        var crates = List.empty[Char]
        for i <- 1 to m.count do {
            val p = arrStack(m.fromPos).pop()
            crates = p :: crates
        }
        for p <- crates do {
            arrStack(m.toPos).push(p)
        }
    }

    var res = Vector.empty[Char]
    for i <- 0 until arrStack.length do {
        res :+= arrStack(i).pop()
    }

    println(new String(res.toArray))
}