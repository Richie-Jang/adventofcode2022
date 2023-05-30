import java.nio.file.Files
import java.nio.file.Paths
object day21 {

    val sample = """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""
    
    enum Sign {
        case Plus, Minus, Mul, Div
    }

    var yellMap = Map.empty[String, Long]
    var monkeyMap = Map.empty[String, (String, String, Sign)]

    def parseLine(s: String): Unit = {
        def divstr(v: String): (String, String, Sign) = {
            val pat = """(\w+) ([\+\-\*\/]) (\w+)""".r
            val mat = pat.findFirstMatchIn(v).get
            val a1 = mat.group(1)
            val a2 = mat.group(3)
            val sign = mat.group(2) match {
                case "+" => Sign.Plus
                case "-" => Sign.Minus
                case "*" => Sign.Mul
                case _ => Sign.Div
            }
            (a1, a2, sign)
        }
        
        val ss = s.split(": ")
        val c = ss(1).toLongOption
        if c.isDefined then 
            yellMap = yellMap.updated(ss(0), c.get)
        else
            monkeyMap = monkeyMap.updated(ss(0), divstr(ss(1)))
    }

    def handleSign(sign: Sign, v1: Long, v2: Long): Long = {
        sign match
            case Sign.Plus => v1 + v2
            case Sign.Minus => v1 - v2
            case Sign.Mul => v1 * v2
            case Sign.Div => v1 / v2        
    }

}

import scala.jdk.CollectionConverters.*

@main def day21Part1: Unit = {
    import day21.*

    yellMap = Map.empty
    monkeyMap = Map.empty

    sample.split("\r\n").foreach(parseLine)
    //Files.readAllLines(Paths.get("./inputs/input_day21.txt")).asScala.foreach(parseLine)

    while !yellMap.contains("root") do {
        var newVals = Set.empty[String]
        val ks = monkeyMap.keys.toList
        for k <- ks do {
            val v = monkeyMap(k)
            if yellMap.contains(v._1) && yellMap.contains(v._2) then {
                newVals += k
                yellMap = yellMap.updated(k, handleSign(v._3, yellMap(v._1), yellMap(v._2)))
            }
        }
        newVals.foreach(k => monkeyMap = monkeyMap.removed(k))
    }

    println(yellMap)
    println(yellMap("root"))
}