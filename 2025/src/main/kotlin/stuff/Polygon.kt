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

    private fun orientation(a: Coordinate, b: Coordinate, c: Coordinate): Int {
        val v = (b.y - a.y) * (c.x - b.x) - (b.x - a.x) * (c.y - b.y)
        return when {
            v > 0 -> 1
            v < 0 -> -1
            else -> 0
        }
    }

    private fun segmentsIntersect(a: Coordinate, b: Coordinate, c: Coordinate, d: Coordinate): Boolean {
        val o1 = orientation(a, b, c)
        val o2 = orientation(a, b, d)
        val o3 = orientation(c, d, a)
        val o4 = orientation(c, d, b)

        if (o1 != o2 && o3 != o4) return true

        if (o1 == 0 && isPointOnSegment(c, a, b)) return true
        if (o2 == 0 && isPointOnSegment(d, a, b)) return true
        if (o3 == 0 && isPointOnSegment(a, c, d)) return true
        if (o4 == 0 && isPointOnSegment(b, c, d)) return true

        return false
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
        /*
        for (i in vertices.indices) {
            val c = vertices[i]
            val d = vertices[(i + 1) % vertices.size]

            if (segmentsIntersect(a, b, c, d)) {
                val touching = isPointOnSegment(a, c, d)
                        || isPointOnSegment(b, c, d)
                if (!touching) {
                    if (a.x == b.x) {
                        for (y in c.y..max(a.y, b.y)) {
                            if (!isPointInside(Coordinate(a.x, y))) {
                                return false
                            }
                        }
                    } else if (a.y == b.y) {
                        for (x in c.x..max(a.x, b.x)) {
                            if (!isPointInside(Coordinate(x, a.y))) {
                                return false
                            }
                        }
                    }
                }
            }
        }

        return true
         */
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