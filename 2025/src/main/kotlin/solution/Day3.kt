package org.example.solution

class Day3 : Solution {
    private var sumJoltage: Long = 0

    private fun processLinePart1(line: String) {
        var tens: Char = line.first()
        var tensIndex = 0

        for (i in 1..line.length - 2) {
            if (tens == '9') {
                break
            }
            if (line[i] > tens) {
                tensIndex = i
                tens = line[i]
            }
        }

        var units: Char = line[tensIndex + 1]

        for (i in tensIndex + 2..<line.length) {
            if (line[i] > units) {
                units = line[i]
            }
        }

        sumJoltage += (tens.toString() + units).toLong()
    }

    private fun processLinePart2(line: String) {
        var batteryJoltage = ""
        var localJoltage: Char
        var localJoltageIndex = -1

        for (i in 1..12) {
            localJoltageIndex++
            localJoltage = line[localJoltageIndex]
            for (j in localJoltageIndex..line.length - 13 + i) {
                if (line[j] > localJoltage) {
                    localJoltageIndex = j
                    localJoltage = line[j]
                }
            }
            batteryJoltage += localJoltage
        }

        sumJoltage += batteryJoltage.toLong()
    }

    override fun partOne(lines: List<String>): String {
        lines.forEach(::processLinePart1)
        return sumJoltage.toString()
    }

    override fun partTwo(lines: List<String>): String {
        lines.forEach(::processLinePart2)
        return sumJoltage.toString()
    }
}