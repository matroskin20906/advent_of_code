package org.example.stuff

import kotlin.math.max
import kotlin.math.min

class Polygon {
    private val vertices = mutableListOf<Coordinate>()

    fun addVertex(coordinate: Coordinate) {
        vertices.add(coordinate)
    }

    private fun isPointOnSegment(p: Coordinate, a: Coordinate, b: Coordinate): Boolean {
        val cross = (p.y - a.y) * (b.x - a.x) - (p.x - a.x) * (b.y - a.y)
        if (kotlin.math.abs(cross) > 1e-9) return false

        val dot = (p.x - a.x) * (b.x - a.x) + (p.y - a.y) * (b.y - a.y)
        if (dot < 0) return false

        val lenSq = (b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)
        return dot <= lenSq
    }

    fun isPointInside(p: Coordinate): Boolean {
        for (i in vertices.indices) {
            val a = vertices[i]
            val b = vertices[(i + 1) % vertices.size]
            if (isPointOnSegment(p, a, b)) return true
        }

        var count = 0
        for (i in vertices.indices) {
            val a = vertices[i]
            val b = vertices[(i + 1) % vertices.size]

            val intersect = (a.y > p.y) != (b.y > p.y) &&
                    p.x < (b.x - a.x) * (p.y - a.y) / (b.y - a.y) + a.x

            if (intersect) count++
        }

        return count % 2 == 1
    }

    fun isSegmentInside(a: Coordinate, b: Coordinate): Boolean {
        if (!isPointInside(a)) return false
        if (!isPointInside(b)) return false

        if (a.x == b.x) {
            for (y in min(a.y, b.y)..max(a.y, b.y)) {
                if (!isPointInside(Coordinate(a.x, y))) {
                    return false
                }
            }
        } else {
            for (x in min(a.x, b.x)..max(a.x, b.x)) {
                if (!isPointInside(Coordinate(x, a.y))) {
                    return false
                }
            }
        }

        return true
    }

    override fun toString(): String {
        if (vertices.isEmpty()) {
            return ""
        }
        var result = ""

        result += vertices.first()

        for (i in 1..<vertices.size - 1) {
            result += ", " + vertices[i]
        }

        result += vertices.last()

        return result
    }
}