package org.example.solution

class Day7 : Solution {
    private var split = 0

    private fun makeOneStepPart1(line: String, beams: Set<Int>) : Set<Int> {
        val newBeams = mutableSetOf<Int>()

        beams.forEach {
            if (line[it] != '^') {
                newBeams.add(it)
                return@forEach
            }
            split += if (it - 1 >= 0 || it + 1 < line.length) 1 else 0
            if (it - 1 >= 0) {
                newBeams.add(it - 1)
            }
            if (it + 1 < line.length) {
                newBeams.add(it + 1)
            }
        }

        return newBeams
    }

    private fun makeOneStepPart2(line: String, beams: Map<Long, Long>) : Map<Long, Long> {
        val newBeams = mutableMapOf<Long, Long>()

        for ((key, value) in beams) {
            if (line[key.toInt()] != '^') {
                newBeams[key] = value + newBeams.getOrDefault(key, 0)
                continue
            }
            if (key - 1 >= 0) {
                newBeams[key - 1] = newBeams.getOrDefault(key - 1, 0) + value
            }
            if (key + 1 < line.length) {
                newBeams[key + 1] = newBeams.getOrDefault(key + 1, 0) + value
            }
        }

        return newBeams
    }

    override fun partOne(lines: List<String>): String {
        var beams = setOf(lines.first().indexOf('S'))

        for (i in 1..<lines.size) {
            beams = makeOneStepPart1(lines[i], beams)
        }

        return split.toString()
    }

    override fun partTwo(lines: List<String>): String {
        var beams = mapOf(lines.first().indexOf('S').toLong() to 1.toLong())

        for (i in 1..<lines.size) {
            beams = makeOneStepPart2(lines[i], beams)
        }

        return beams.values.sum().toString()
    }
}