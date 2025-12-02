package org.example.solution

import org.example.stuff.Direction

class Day1 : Solution {
    private var position: Int = 50
    private var password: Int = 0

    private fun parseLine(line: String): Pair<Direction, Int> {
        val direction = Direction.fromChar(line[0])
        val step: Int = line.drop(1).toInt()

        return Pair(direction, step)
    }

    override fun partOne(lines: List<String>): String {
        for (line in lines) {
            val (direction, step) = parseLine(line)
            position = when (direction) {
                Direction.LEFT -> (100 + position - step) % 100
                Direction.RIGHT -> (position + step) % 100
            }
            if (position == 0)
                password++
        }

        return password.toString()
    }

    override fun partTwo(lines: List<String>): String {
        for (line in lines) {
            val pair = parseLine(line)
            val direction = pair.first
            var step = pair.second
            if (step > 100) {
                password += step / 100
                step %= 100
            }
            position = when (direction) {
                Direction.LEFT -> {
                    if (position != 0 && position - step < 0)
                        password++
                    (100 + position - step) % 100
                }

                Direction.RIGHT -> {
                    if (position != 0 && position + step > 100)
                        password++
                    (position + step) % 100
                }
            }
            if (position == 0)
                password++
        }

        return password.toString()
    }
}