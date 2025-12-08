package org.example.solution

import org.example.stuff.Operation

class Day6 : Solution {
    private fun getOperations(lines: List<String>): MutableList<Operation> =
        lines.last().trim().replace("\\s+".toRegex(), " ").split(" ").map {
            when (it) {
                "+" -> Operation.PLUS
                "*" -> Operation.MULT
                else -> Operation.MULT
            }
        } as MutableList<Operation>

    private fun getNumbersPart1(line: String): List<Long> =
        line.trim().replace("\\s+".toRegex(), " ").split(" ").map {
            it.toLong()
        }

    override fun partOne(lines: List<String>): String {
        val operations = getOperations(lines)

        val results = mutableListOf<Long>()
        val firstNumbers = getNumbersPart1(lines.first())
        for (i in 0..<firstNumbers.size) {
            results.add(i, firstNumbers[i])
        }

        for (i in 1..<(lines.size - 1)) {
            val numbers = getNumbersPart1(lines[i])
            require(numbers.size == operations.size)

            for (i in 0..<numbers.size) {
                results[i] = operations[i].apply(results[i], numbers[i])
            }
        }

        return results.sum().toString()
    }

    override fun partTwo(lines: List<String>): String {
        val operations = getOperations(lines)

        var j = lines.first().length
        var operation = operations.removeLast()
        var result: Long = 0
        var sum: Long = when (operation) {
            Operation.MULT -> 1
            Operation.PLUS -> 0
        }
        while (j > 0) {
            j--
            var number : Long = 0
            for (i in 0..<lines.size - 1) {
                number = if (lines[i][j].toString() == " ") {
                    number
                } else {
                    number * 10 + lines[i][j].toString().toLong()
                }
            }
            if (number == 0.toLong()) {
                result += sum
                operation = operations.removeLast()
                sum = when (operation) {
                    Operation.MULT -> 1
                    Operation.PLUS -> 0
                }
            } else {
                sum = operation.apply(sum, number)
            }
        }
        result += sum

        return result.toString()
    }
}