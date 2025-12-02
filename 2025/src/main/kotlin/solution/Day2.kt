package org.example.solution

class Day2 : Solution {
    private var invalidIdSum: Long = 0

    private fun processNumberPart1(number: Long) {
        val numberAsString = number.toString()
        if (numberAsString.length % 2 != 0) {
            return
        }
        val firstPart = numberAsString.take(numberAsString.length / 2)
        val secondPart = numberAsString.substring(numberAsString.length / 2, numberAsString.length)

        if (firstPart != secondPart) {
            return
        }

        println("number $number is invalid ID")
        invalidIdSum += number
    }

    private fun processNumberPart2(number: Long) {
        val numberAsString = number.toString()
        var numberPart = ""
        for (digit in numberAsString) {
            numberPart += digit.toString()
            if (numberPart.length == numberAsString.length) {
                return
            }

            if (numberAsString.length % numberPart.length != 0) {
                continue
            }

            if (numberAsString == numberPart.repeat(numberAsString.length / numberPart.length)) {
                println("number $number is invalid ID")
                invalidIdSum += number
                return
            }
        }
    }

    private fun processOneRange(from: Long, to: Long, processFunction: (number: Long) -> Unit) {
        for (i in from..to) {
            processFunction(i)
        }
    }

    override fun partOne(lines: List<String>): String {
        val idRangeLine: String = lines[0]
        val idRanges = idRangeLine.split(",")
        idRanges.forEach {
            val splitLine = it.split("-")
            processOneRange(splitLine[0].toLong(), splitLine[1].toLong(), ::processNumberPart1)
        }

        return invalidIdSum.toString()
    }

    override fun partTwo(lines: List<String>): String {
        val idRangeLine: String = lines[0]
        val idRanges = idRangeLine.split(",")
        idRanges.forEach {
            val splitLine = it.split("-")
            processOneRange(splitLine[0].toLong(), splitLine[1].toLong(), ::processNumberPart2)
        }

        return invalidIdSum.toString()
    }
}