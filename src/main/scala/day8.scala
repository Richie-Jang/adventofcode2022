import java.nio.file.{Files, Paths}

object day8 {

    val sample = """30373
                   |25512
                   |65332
                   |33549
                   |35390""".stripMargin

    type Arr2D = Array[Array[Int]]

    def make2DArr(input: String): Arr2D = {
        input.split("\n").map{a =>
            a.trim.map{ v => Character.digit(v, 10) }.toArray
        }
    }

    def print2DArr(arr: Arr2D): Unit = {
        arr.foreach{ v =>
            println(v.mkString(" "))
        }
    }

    def getEdgeTreesCount(arr: Arr2D): Int = {
        val h = arr.length
        val w = arr(0).length
        var sum = 0
        for {
            y <- 0 until h
            x <- 0 until w
        } do {
            if y == 0 || y == h-1 || x == 0 || x == w-1 then sum += 1
        }
        sum
    }

    def getCountInteriorTrees(arr: Arr2D): Int = {
        val h = arr.length
        val w = arr(0).length

        def isVisibleLeft(cy: Int, cx: Int, cur: Int): Boolean = {
            (0 until cx).forall{v =>
                arr(cy)(v) < cur
            }
        }

        def isVisibleRight(cy: Int, cx: Int, cur: Int): Boolean = {
            (cx+1 until w).forall { v =>
                arr(cy)(v) < cur
            }
        }

        def isVisibleTop(cy: Int, cx: Int, cur: Int): Boolean = {
            (0 until cy).forall { v =>
                arr(v)(cx) < cur
            }
        }

        def isVisibleBottom(cy: Int, cx: Int, cur: Int): Boolean = {
            (cy+1 until h).forall { v =>
                arr(v)(cx) < cur
            }
        }

        var sum = 0

        for {
            y <- 1 until h-1
            x <- 1 until w-1
        } do {
            val cur = arr(y)(x)
            val c1 = isVisibleLeft(y,x,cur)
            val c2 = isVisibleRight(y,x,cur)
            val c3 = isVisibleTop(y,x,cur)
            val c4 = isVisibleBottom(y,x,cur)
            if List(c1, c2, c3, c4).contains(true) then sum += 1
        }

        sum
    }

    
    def searchHighestScenicScoreTree(arr: Arr2D): Int = {
        val w = arr(0).length
        val h = arr.length

        def searchUp(cur: Int, x: Int, y: Int, acc: Int): Int = {
            if y < 0 then return acc
            if arr(y)(x) < cur then return searchUp(cur, x, y-1, acc+1)
            acc+1
        }
        def searchDown(cur: Int, x: Int, y: Int, acc: Int): Int = {
            if y >= h then return acc
            if arr(y)(x) < cur then return searchDown(cur, x, y+1, acc+1)
            acc+1            
        }
        def searchLeft(cur: Int, x: Int, y: Int, acc: Int): Int = {
            if x < 0 then return acc
            if arr(y)(x) < cur then return searchLeft(cur, x-1, y, acc+1)
            acc+1            
        }
        def searchRight(cur: Int, x: Int, y: Int, acc: Int): Int = {
            if x >= w then return acc
            if arr(y)(x) < cur then return searchRight(cur, x+1, y, acc+1)
            acc+1            
        }

        var res = 0

        for 
            y <- 1 to h-2
            x <- 1 to w-2
        do {                     
            val a1 = searchUp(arr(y)(x), x, y-1, 0)
            val a3 = searchDown(arr(y)(x), x, y+1, 0)
            val a2 = searchLeft(arr(y)(x), x-1, y, 0)
            val a4 = searchRight(arr(y)(x), x+1, y, 0)

            val sum = List(
                a1,a2,a3,a4
            ).fold(1)(_*_)
            if res < sum then res = sum
        }
        res
    }
}

@main def day8Part1: Unit = {
    import day8.*
    val inputStr = {
        //Files.readString(Paths.get("./inputs/input_day8.txt"))
        new String(Files.readAllBytes(Paths.get("./inputs/input_day8.txt")))
    }
    val arr = make2DArr(inputStr)
    val edges = getEdgeTreesCount(arr)
    print2DArr(arr)
    val total = edges + getCountInteriorTrees(arr)
    println(total)
}

@main def day8Part2: Unit = {
    import day8.*
    val inputStr = {
        //Files.readString(Paths.get("./inputs/input_day8.txt"))
        new String(Files.readAllBytes(Paths.get("./inputs/input_day8.txt")))
    }
    val arr = make2DArr(inputStr)
    println(searchHighestScenicScoreTree(arr))
}