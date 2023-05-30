import java.io.BufferedReader
import java.io.FileReader
@main def day6: Unit = {

    val br = new BufferedReader(new FileReader("./inputs/input_day6.txt"))
    val value = br.readLine()
    val packetLength = value.length

    //val test1 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    //val packetLength = test1.length

    // part1
    def checkMarkerWith4(str: String, curPos: Int): Int = {
        val endPos = curPos + 4
        if endPos > packetLength then return Int.MaxValue
        val cur = str.slice(curPos, endPos)
        val set = cur.toSet
        if set.size == 4 then {
            // found
            endPos
        } else {
            checkMarkerWith4(str, curPos+1)
        }
    }

    // println(checkMarkerWith4(value, 0))

    // part2
    def checkMarkerWith14(str: String, curPos: Int): Int = {
        val endPos = curPos + 14
        if endPos > packetLength then return Int.MaxValue
        val cur = str.slice(curPos, endPos)
        val set = cur.toSet
        if set.size == 14 then {
            // found
            endPos
        } else {
            checkMarkerWith14(str, curPos + 1)
        }
    }

    println(checkMarkerWith14(value, 0))

}