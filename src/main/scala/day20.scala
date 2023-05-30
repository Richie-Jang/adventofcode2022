import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.util.chaining.*
object day20 {

    val sample = """1
                   |2
                   |-3
                   |3
                   |-2
                   |0
                   |4""".stripMargin

    case class NumIndex(value: Int, index: Int)

    def createNodesFromStr(str: String): ArrayBuffer[NumIndex] = {
        val ss =
            str.split("\r\n")
                .map { a => a.toInt }
                .zipWithIndex
                .map { (a, i) => NumIndex(a, i) }
        ArrayBuffer.from(ss)
    }

    def decrypt(nodes: ArrayBuffer[NumIndex]): Unit = {
        val len = nodes.length - 1
        for i <- nodes.indices do {
            val index = nodes.indexWhere(a => a.index == i)
            val moved = nodes.remove(index)
            var newIndex = (index + moved.value) % len
            if newIndex < 0 then newIndex = len + newIndex
            nodes.insert(newIndex, moved)

            // println(nodes)
        }
    }

    def groveCoordinates(nodes: ArrayBuffer[NumIndex]): Long = {
        val zero = nodes.indexWhere(p => p.value == 0)
        List(1000, 2000, 3000).map { a =>
            nodes((zero + a) % (nodes.length)).value.toLong
        }.sum
    }

}

@main def day20Part1(): Unit = {
    import day20.*

    // val nodes = createNodesFromStr(sample)
    val nodes = createNodesFromStr(
      Files.readString(Paths.get("./inputs/input_day20.txt"))
    )

    decrypt(nodes)
    println(groveCoordinates(nodes))
}
