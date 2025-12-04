package org.example.solution

import org.example.stuff.Board

class Day4 : Solution {
    override fun partOne(lines: List<String>): String {
        val board = Board(lines, '@')
        val accessibleRolls = board.getCoordinatesByCondition {
            board.isPointOfInterest(it) && board.neighborsCount(it) < 4
        }

        return accessibleRolls.size.toString()
    }

    override fun partTwo(lines: List<String>): String {
        val board = Board(lines, '@')
        var accessibleRolls = board.getCoordinatesByCondition {
            board.isPointOfInterest(it) && board.neighborsCount(it) < 4
        }
        var accessibleRollsCount = 0
        while (!accessibleRolls.isEmpty()) {
            accessibleRollsCount += accessibleRolls.size
            board.removePointsOfInterest(accessibleRolls)
            accessibleRolls = board.getCoordinatesByCondition {
                board.isPointOfInterest(it) && board.neighborsCount(it) < 4
            }
        }

        return accessibleRollsCount.toString()
    }
}