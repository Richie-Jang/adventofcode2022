import java.nio.file.{Files, Paths}

object day13 {

  // refer from the other solution.. I could not solve it by myself.
  val sample: String = """[1,1,3,1,1]
                 |[1,1,5,1,1]
                 |
                 |[[1],[2,3,4]]
                 |[[1],4]
                 |
                 |[9]
                 |[[8,7,6]]
                 |
                 |[[4,4],4,4]
                 |[[4,4],4,4,4]
                 |
                 |[7,7,7,7]
                 |[7,7,7]
                 |
                 |[]
                 |[3]
                 |
                 |[[[]]]
                 |[[]]
                 |
                 |[1,[2,[3,[4,[5,6,7]]]],8,9]
                 |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin
  /**    
    * change 10 => A single char and handle it
    * @param input
    * @return
    */
  def simplify(input: Seq[String]): Seq[String] =
    input.filter(_.nonEmpty).map(_.replace("10", "A"))

  def compare(left: String, right: String): Boolean = {
    (left.head, right.head) match {
      case (a, b) if a == b => compare(left.tail, right.tail)
      case (']', _)         => true
      case (_, ']')         => false
      case ('[', b)         => compare(left.tail, b + "]" + right.tail)
      case (a, '[')         => compare(a + "]" + left.tail, right.tail)
      case (a, b)           => a < b
    }
  }

}

@main def day13Part1: Unit = {
  import day13.*
  import scala.jdk.CollectionConverters.*
  //val i2 = simplify(sample.split("\r\n")).grouped(2).zipWithIndex
  val i2 = simplify(Files.readAllLines(Paths.get("./inputs/input_day13.txt"))
      .asScala.toSeq)
    .grouped(2).zipWithIndex
  val res = i2.collect { case (Seq(left, right), index) if compare(left, right) => index + 1 }.sum
  println(res)
}

@main def day13Part2: Unit = {
  import day13.*
  import scala.jdk.CollectionConverters.*

  //val input = simplify(sample.split("\r\n"))
  val input = simplify(Files.readAllLines(Paths.get("./inputs/input_day13.txt")).asScala.toSeq)
  val dividerPackets = Seq("[[2]]", "[[6]]")
  val sorted = (input ++ dividerPackets).sortWith(compare)

  var vs = Vector.empty[Int]
  var index = 1
  for s <- sorted do {
    if dividerPackets.contains(s) then vs :+= index
    index += 1
  }  

  println(vs.fold(1)(_*_))
}
