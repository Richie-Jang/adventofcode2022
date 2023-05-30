import java.nio.file.{Files, Paths}

object day13_2 {

  enum NodeType {
    case Num, Arr
  }

  import NodeType.*

  class Node(var nodeType: NodeType,
             val rep: String,
             val value: Int,
             var children: Vector[Node],
             val special: Int) {
    override def toString: String = {
      s"Nt: $nodeType, {$rep}, v:$value, [ $children ], $special"
    }
  }

  def compare(a: Int, b: Int): Int = if a < b then 1 else if a > b then -1 else 0

  def compareNodes(n1: Node, n2: Node): Int = {
    (n1.nodeType, n2.nodeType) match {
      case (Num, Num) => compare(n1.value, n2.value)

    }
  }


  def parseList(inp: String): Node = {
    val n = new Node(Arr, inp, -1, Vector.empty, 0)
    val len = inp.length

    def loopEndBracket(j: Int, openBk: Int, starter: Int): Int = {
      if openBk == 0 then {
        val rstr = inp.slice(starter, j)
        n.children :+= parseList(rstr)
        return j
      }

      if j >= len then return j

      val c = inp(j)
      c match {
        case '[' => loopEndBracket(j+1, openBk + 1, starter)
        case ']' => loopEndBracket(j+1, openBk - 1, starter)
        case _ => loopEndBracket(j+1, openBk, starter)
      }
    }

    def loopForNum(j: Int, starter: Int): Int = {
      if j >= len then return j
      val g = inp(j)
      if g == ',' || g == ']' then {
        n.children :+= parseNumber(inp.slice(starter, j))
        j
      } else {
        loopForNum(j+1, starter)
      }
    }

    def loop(i: Int): Unit = {
      if i >= len then return
      val c = inp(i)
      c match {
        // start-bracket
        case '[' =>
          loop(loopEndBracket(i+1, 1, i))
        // comma
        case ',' => loop(i+1)
        // num
        case _ => loop(loopForNum(i+1, i))
      }
    }

    loop(1)
    n
  }

  def parseNumber(inp: String): Node = {
    Node(Num, inp, inp.toInt, Vector.empty, 0)
  }

  def convertSeqTwoNodes(inp: Seq[String]): Seq[(Node, Node)] = {
    var res = Vector.empty[(Node, Node)]
    // side : left = 0, right = 1
    def loop(sq: Seq[String], side: Int, acc: Vector[Node]): Unit = {
      if sq.isEmpty then return
      val l = sq.head
      if l == "" then {
        res :+= (acc(0), acc(1))
        return loop(sq.tail, 0, Vector.empty)
      }
      if side == 0 then
        loop(sq.tail, 1, Vector(parseList(l)))
      else
        loop(sq.tail, 0, acc :+ parseList(l))
    }

    loop(inp, 0, Vector.empty)
    res
  }

}


import scala.jdk.CollectionConverters.*
@main def day13_2_part1_trial: Unit = {

  import day13_2.*

  /*val inp = Files.readAllLines(Paths.get("./inputs/input_day13.txt")).asScala.toSeq
  val data: Seq[(Node, Node)] = convertSeqTwoNodes(inp)
  for v <- data do {
    println(v)
  }*/

  val t1 = "[[4,4],4,4]"
  val p1 = parseList(t1)

  println(p1)
}