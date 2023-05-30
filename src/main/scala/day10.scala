import java.nio.file.Files
import java.nio.file.Paths
object day10 {

  val sample = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""

  val selects = List(20, 60, 100, 140, 180, 220)

  enum Op {
    case NO, ADD
  }

  def parseLine(s: String): (Op, Int) = {
    val ss = s.split(' ')
    val op =
      ss(0) match {
        case "noop" => Op.NO
        case _      => Op.ADD
      }
    if op == Op.ADD then return op -> (ss(1).toInt)
    op -> 0
  }

}

import scala.jdk.CollectionConverters.*

@main def day10Part1: Unit = {
  import day10.*

  val inputs = sample.split("\n").map(a => parseLine(a.trim))

  /* val inputs = Files.readAllLines(Paths.get("./inputs/input_day10.txt"))
    .asScala.map(parseLine) */

  var count = 0
  def updateCount(op: Op, v: Int = 0): Unit = {
    count += 1
    if count == 220 then {
      println(s"break: $op $v")
    } 
  }

  var x = 1

  var map = scala.collection.mutable.Map.empty[Int, Int]
  map(0) = 1

  for (op, c) <- inputs do {
    updateCount(op)
    map(count) = x
    op match {
      case Op.NO =>         
        ()
      case Op.ADD =>                 
        updateCount(op,c)
        map(count) = x
        x += c                
    }
  }

  var total = 0
  for i <- selects do {    
    println(s"$i => ${map(i)}")
    total += (i * map(i))
  }
  println(total)
  println(s"Count: $count")
}


@main def day10Part2: Unit = {
  import day10.*

  //val inputs = sample.split("\n").map(a => parseLine(a.trim))
  
  val inputs = Files.readAllLines(Paths.get("./inputs/input_day10.txt"))
    .asScala.map(parseLine).toList

  val crt = Array.ofDim[Char](6, 40)
  var cycle = -1  
  val masking = Array.ofDim[Char](40)
  var x = 1

  def updateCycle: Unit = {
    cycle += 1  
    // check masking
    val y = cycle / 40    
    val x = {
      if cycle == 0 then 0 else cycle % 40
    }

    if masking(x) == '#' then 
      crt(y)(x) = '#'
    else 
      crt(y)(x) = '.'
  }

  def updateMasking(v: Int): Unit = {
    // update by x
    for i <- 0 until 40 do masking(i) = '.'
    x += v

    def fillSharp(idx: Int): Unit = {      
      if idx < 0 then {
        val vv = 40 + idx
        masking(vv) = '#'
      } else if idx >= 40 then {
        val vv = idx - 40
        masking(vv) = '#'
      } else {
        masking(idx) = '#'
      }
    }

    fillSharp(x-1)
    fillSharp(x)
    fillSharp(x+1)
  }

  // init x = 1 
  updateMasking(0)

  for (op, c) <- inputs do {
    updateCycle
    if op == Op.ADD then {      
      updateCycle
      updateMasking(c)
    }
  }

  crt.foreach{ p => 
    println(p.mkString(""))
  }
}