package org.example

import org.example.solution.Day1
import org.example.solution.Day2
import org.example.solution.Day3
import org.example.solution.Day4
import org.example.solution.Day5
import org.example.solution.Day6
import org.example.solution.Day7
import org.example.solution.Day8
import org.example.solution.Day9
import org.example.solution.Solution
import java.io.File
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    if (args.size < 3) {
        println("u should give day as first argument, part as second and input file as third")
        exitProcess(1)
    }
    val day = args[0]
    val part = args[1]
    val inputFile = args[2]

    val solution: Solution = when (day) {
        "1" -> Day1()
        "2" -> Day2()
        "3" -> Day3()
        "4" -> Day4()
        "5" -> Day5()
        "6" -> Day6()
        "7" -> Day7()
        "8" -> Day8()
        "9" -> Day9()
        else -> TODO("not implemented")
    }

    val lines = File(inputFile).useLines { it.toList() }

    when (part) {
        "1" -> println(solution.partOne(lines))
        "2" -> println(solution.partTwo(lines))
        else -> error("such thing do not exists")
    }
}
