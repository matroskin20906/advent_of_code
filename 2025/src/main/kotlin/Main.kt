package org.example

import org.example.solution.Day1
import org.example.solution.Day2
import org.example.solution.Solution
import java.io.File
import kotlin.system.exitProcess

fun main(args : Array<String>) {
    if (args.size < 3) {
        println("u should give day as first argument, part as second and input file as third")
        exitProcess(1)
    }
    val day = args[0]
    val part = args[1]
    val inputFile = args[2]

    val solution : Solution = when (day) {
        "1" -> Day1()
        "2" -> Day2()
        else -> TODO("not implemented")
    }

    val lines = File(inputFile).useLines { it.toList() }

    when (part) {
        "1" -> println(solution.partOne(lines))
        "2" -> println(solution.partTwo(lines))
        else -> error("such thing do not exists")
    }
}
