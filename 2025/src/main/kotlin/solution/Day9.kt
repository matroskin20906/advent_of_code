package org.example.solution

import org.example.stuff.Coordinate
import org.example.stuff.Edge2d
import org.example.stuff.Polygon
import kotlin.math.max
import kotlin.math.min

class Day9 : Solution {
    override fun partOne(lines: List<String>): String {
        val coordinates = getCoordinates(lines)
        val convexHull = convexHull(coordinates)
        val edges = mutableListOf<Edge2d>()

        convexHull.forEach { from ->
            convexHull.forEach { to ->
                edges.add(Edge2d(from, to))
            }
        }

        edges.sortByDescending { it.rectangleArea }

        return edges.first().rectangleArea.toString()
    }

    override fun partTwo(lines: List<String>): String {
        val coordinates = getCoordinates(lines)
        val polygon = Polygon()
        coordinates.forEach { polygon.addVertex(it) }
        val edges = mutableSetOf<Edge2d>()
        coordinates.forEach { from ->
            coordinates.forEach { to ->
                if (from != to)
                    edges.add(Edge2d(from, to))
            }
        }

        val rectangleWithVerticesInsidePolygon = edges.filter {
            val minX = min(it.from.x, it.to.x)
            val minY = min(it.from.y, it.to.y)
            val maxX = max(it.from.x, it.to.x)
            val maxY = max(it.from.y, it.to.y)

            polygon.isPointInside(Coordinate(minX, minY))
                    && polygon.isPointInside(Coordinate(minX, maxY))
                    && polygon.isPointInside(Coordinate(maxX, minY))
                    && polygon.isPointInside(Coordinate(maxX, maxY))
        }

        val rectanglesWithBordersInsidePolygon = rectangleWithVerticesInsidePolygon.filter {
            val minX = min(it.from.x, it.to.x)
            val minY = min(it.from.y, it.to.y)
            val maxX = max(it.from.x, it.to.x)
            val maxY = max(it.from.y, it.to.y)

            polygon.isSegmentInside(Coordinate(minX, minY), Coordinate(minX, maxY))
                    && polygon.isSegmentInside(Coordinate(minX, maxY), Coordinate(maxX, maxY))
                    && polygon.isSegmentInside(Coordinate(maxX, maxY), Coordinate(maxX, minY))
                    && polygon.isSegmentInside(Coordinate(maxX, minY), Coordinate(minX, minY))
        }

        return rectanglesWithBordersInsidePolygon.maxByOrNull { it.rectangleArea }!!.rectangleArea.toString()
    }

    private fun getCoordinates(lines: List<String>): List<Coordinate> =
        lines.map {
            val split = it.split(",")
            Coordinate(split[0].toInt(), split[1].toInt())
        }

    private fun convexHull(coordinates: List<Coordinate>): List<Coordinate> {
        if (coordinates.size <= 1)
            return coordinates

        val sorted = coordinates.sortedWith(compareBy({ it.x }, { it.y }))
        val lower = mutableListOf<Coordinate>()
        for (p in sorted) {
            while (lower.size >= 2 && lower[lower.size - 1].crossProduct(lower[lower.size - 2], p) <= 0) {
                lower.removeAt(lower.size - 1)
            }
            lower.add(p)
        }

        val upper = mutableListOf<Coordinate>()
        for (p in sorted.asReversed()) {
            while (upper.size >= 2 && upper[upper.size - 1].crossProduct(upper[upper.size - 2], p) <= 0) {
                upper.removeAt(upper.size - 1)
            }
            upper.add(p)
        }

        return (lower.dropLast(1) + upper.dropLast(1))
    }
}