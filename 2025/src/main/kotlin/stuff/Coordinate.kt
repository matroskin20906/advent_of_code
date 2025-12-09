package org.example.stuff

import kotlin.math.sqrt

data class Coordinate(val x: Int, val y: Int) {
    operator fun plus(other: Coordinate) : Coordinate {
        return Coordinate(x + other.x, y + other.y)
    }

    operator fun minus(other: Coordinate) : Coordinate {
        return Coordinate(x - other.x, y - other.y)
    }

    fun distance(to: Coordinate) : Double {
        val deltaX = x - to.x
        val deltaY = y - to.y

        return sqrt((deltaX * deltaX + deltaY * deltaY).toDouble())
    }

    fun crossProduct(center: Coordinate, b: Coordinate) : Int =
        (x - center.x) * (b.y - center.y) - (y - center.y) * (b.x - center.x)
}
