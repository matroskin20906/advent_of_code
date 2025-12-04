package org.example.stuff

class Board(lines: List<String>, pointOfInterest: Char) {
    private val pointOfInterestValue = 1

    val width: Int = lines.first().length
    val height: Int = lines.size
    private val board = Array(width) { IntArray(height) }

    init {
        for (i in 0..<lines.size) {
            for (j in 0..<lines[i].length) {
                if (lines[i][j] == pointOfInterest) {
                    board[i][j] = pointOfInterestValue
                }
            }
        }
    }

    fun getCoordinatesByCondition(condition: (Coordinate) -> Boolean) : List<Coordinate> {
        val coords = mutableListOf<Coordinate>()
        for (i in 0..<height) {
            for (j in 0..<width) {
                val coordinate = Coordinate(i, j)
                if (condition(coordinate)) {
                    coords.add(coordinate)
                }
            }
        }

        return coords
    }

    fun removePointsOfInterest(coordinates: List<Coordinate>) {
        coordinates.forEach {
            if (isCoordinateOnBoard(it)) {
                board[it.x][it.y] = 0
            }
        }
    }

    fun neighborsCount(coordinate: Coordinate) : Int {
        if (!isCoordinateOnBoard(coordinate)) {
            return 0
        }

        var neighborsCount = 0
        for (compDir in CompassDirection.entries) {
            val newCoord = coordinate + compDir.value
            if (isCoordinateOnBoard(newCoord)) {
                if (isPointOfInterest(newCoord)) {
                    neighborsCount++
                }
            }
        }

        return neighborsCount
    }

    fun isPointOfInterest(coordinate: Coordinate) : Boolean {
        return board[coordinate.x][coordinate.y] == pointOfInterestValue
    }

    fun print() {
        for (i in 0..<height) {
            for (j in 0..<width) {
                print("${board[i][j]} ")
            }
            println()
        }
    }

    private fun isCoordinateOnBoard(coordinate: Coordinate) : Boolean {
        if (coordinate.x !in 0..<width) {
            return false
        }

        if (coordinate.y !in 0..<height) {
            return false
        }

        return true
    }
}