import java.io.{BufferedReader, FileReader, Reader, StringReader}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.StreamConverters.*

object day7 {

    val sample = """$ cd /
                   |$ ls
                   |dir a
                   |14848514 b.txt
                   |8504156 c.dat
                   |dir d
                   |$ cd a
                   |$ ls
                   |dir e
                   |29116 f
                   |2557 g
                   |62596 h.lst
                   |$ cd e
                   |$ ls
                   |584 i
                   |$ cd ..
                   |$ cd ..
                   |$ cd d
                   |$ ls
                   |4060174 j
                   |8033020 d.log
                   |5626152 d.ext
                   |7214296 k""".stripMargin

    case class File(name: String, size: Long)

    class Tree(val foldName: String, var files: Vector[File], var dirs: Vector[Tree], var parent: Tree = null)

    def createTree(stream: BufferedReader): Tree = {
        // root
        val res = new Tree(foldName = "root", files = Vector.empty, dirs = Vector.empty)
        var curTree: Tree = res

        def handleCommand(s: String): Unit = {
            val s1 = s.replace("$ ", "")
            s1 match {
                case a if a.startsWith("cd") && a.contains("/") => curTree = res
                case a if a.startsWith("cd") && a.contains("..") =>
                    curTree = curTree.parent
                case a if a.startsWith("cd") =>
                    val s2 = a.split(' ')
                    val fn = s2(1)
                    curTree = curTree.dirs.find(p => p.foldName == fn).get
                case _ => ()
            }
        }

        def handleDir(s: String): Unit = {
            // update dirs
            val s1 = s.split(' ')
            val newTree = new Tree(s1(1), Vector.empty, Vector.empty, curTree)
            curTree.dirs :+= newTree
        }

        def handleFile(s: String): Unit = {
            val s1 = s.split(' ')
            val size = s1(0).toLong
            val name = s1(1)
            curTree.files :+= File(name, size)
        }

        for line <- stream.lines().toScala(Seq) do {
            line match {
                case a if a.startsWith("$") => handleCommand(a)
                case a if a.startsWith("dir") => handleDir(a)
                case a => handleFile(a)
            }
        }
        res
    }

    var part1Result = 0L
    val MaxSpace = 70000000L
    val NeedSpace = 30000000L

    def printTree(root: Tree): Long = {
        var sum = 0L
        if root.dirs.nonEmpty then {
            for r <- root.dirs do {
                sum += printTree(r)
            }
        }
        val fileSum = root.files.map(v => v.size).sum
        sum += fileSum
        println(s"${root.foldName} => ${sum}")
        if sum <= 100_000 then {
            part1Result += sum
        }
        sum
    }

    def printTree2(root: Tree, requiredSpace: Long, mutAcc: ArrayBuffer[Long]): Long = {
        var sum = 0L
        if root.dirs.nonEmpty then {
            for r <- root.dirs do {
                sum += printTree2(r, requiredSpace, mutAcc)
            }
        }
        val fileSum = root.files.map(v => v.size).sum
        sum += fileSum
        //println(s"${root.foldName} => ${sum}")

        if sum >= requiredSpace then mutAcc.addOne(sum)
        sum
    }

    @main def day7Part1(): Unit = {

        val stream = new BufferedReader(new StringReader(sample))
        //val stream = new BufferedReader(new FileReader("./inputs/input_day7.txt"))
        val root = createTree(stream)
        printTree(root)
        println(part1Result)
        stream.close()
    }

    @main def day7Part2(): Unit = {
        //val stream = new BufferedReader(new StringReader(sample))
        val stream = new BufferedReader(new FileReader("./inputs/input_day7.txt"))
        val root = createTree(stream)

        //val rootValue = 48381165
        val rootValue = 45717263
        // 24_282_737
        val space = MaxSpace - rootValue
        println(s"Need Space: ${space}")
        val mutVals = ArrayBuffer.empty[Long]
        val requreSpaceAtLeast = NeedSpace - space
        println(s"At Least required: ${requreSpaceAtLeast}")
        printTree2(root, requreSpaceAtLeast, mutVals)
        println(mutVals.sorted.head)

        stream.close()
    }

}
