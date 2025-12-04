package org.example.stuff

data class Coordinate(val x: Int, val y: Int) {
    operator fun plus(other: Coordinate) : Coordinate {
        return Coordinate(x + other.x, y + other.y)
    }

    operator fun minus(other: Coordinate) : Coordinate {
        return Coordinate(x - other.x, y - other.y)
    }
}
