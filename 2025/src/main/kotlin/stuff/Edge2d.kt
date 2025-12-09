package org.example.stuff

import kotlin.math.abs

data class Edge2d(val from: Coordinate, val to: Coordinate) {
    val rectangleArea: Long = (abs(from.x - to.x) + 1).toLong() * (abs(from.y - to.y) + 1).toLong()

    override fun toString(): String {
        return "$from -> $to = $rectangleArea"
    }

    override fun hashCode(): Int {
        return from.hashCode() + to.hashCode()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Edge2d

        if (rectangleArea != other.rectangleArea) return false
        if (from == other.from && to == other.to) return true
        if (to == other.from && from == other.to) return true

        return false
    }
}
