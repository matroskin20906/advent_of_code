package org.example.solution

import org.example.stuff.Coordinate3d
import org.example.stuff.Edge3d
import java.util.PriorityQueue

class Day8 : Solution {
    override fun partOne(lines: List<String>): String {
        val minHeap = makeMinHeap(getCoordinates3d(lines))
        val circuits = mutableListOf<MutableSet<Coordinate3d>>()
        for (i in 0..<2000) {
            val minEdge3d = minHeap.remove()
            val fromCircuit = circuits.firstOrNull { it.contains(minEdge3d.fromCoordinate3d) }
            val toCircuit = circuits.firstOrNull { it.contains(minEdge3d.toCoordinate3d) }

            if (fromCircuit != null && toCircuit == null) {
                fromCircuit.add(minEdge3d.toCoordinate3d)
                continue
            }

            if (fromCircuit == null && toCircuit != null) {
                toCircuit.add(minEdge3d.fromCoordinate3d)
                continue
            }

            if (fromCircuit == null && toCircuit == null) {
                circuits.add(mutableSetOf(minEdge3d.fromCoordinate3d, minEdge3d.toCoordinate3d))
                continue
            }

            if (fromCircuit == toCircuit) {
                continue
            }

            if (fromCircuit != null && toCircuit != null) {
                fromCircuit.addAll(toCircuit)
                toCircuit.clear()
            }
        }
        circuits.sortByDescending { it.size }
        var result: Long = 1
        circuits.subList(0, 3).forEach {
            result *= it.size
        }

        return result.toString()
    }

    override fun partTwo(lines: List<String>): String {
        val coordinates = getCoordinates3d(lines)
        val minHeap = makeMinHeap(coordinates)

        var lastConnectionEdge: Edge3d? = null
        val circuits = mutableListOf<MutableSet<Coordinate3d>>()
        while (circuits.isEmpty() || circuits.first().size != coordinates.size) {
            val minEdge3d = minHeap.remove()
            lastConnectionEdge = minEdge3d
            val fromCircuit = circuits.firstOrNull { it.contains(minEdge3d.fromCoordinate3d) }
            val toCircuit = circuits.firstOrNull { it.contains(minEdge3d.toCoordinate3d) }

            if (fromCircuit != null && toCircuit == null) {
                fromCircuit.add(minEdge3d.toCoordinate3d)
                continue
            }

            if (fromCircuit == null && toCircuit != null) {
                toCircuit.add(minEdge3d.fromCoordinate3d)
                continue
            }

            if (fromCircuit == null && toCircuit == null) {
                circuits.add(mutableSetOf(minEdge3d.fromCoordinate3d, minEdge3d.toCoordinate3d))
                continue
            }

            if (fromCircuit == toCircuit) {
                continue
            }

            if (fromCircuit != null && toCircuit != null) {
                fromCircuit.addAll(toCircuit)
                circuits.remove(toCircuit)
            }
        }
        require(lastConnectionEdge != null)
        println(lastConnectionEdge)

        return (lastConnectionEdge.fromCoordinate3d.x.toLong() * lastConnectionEdge.toCoordinate3d.x.toLong()).toString()
    }

    private fun makeMinHeap(coordinates: List<Coordinate3d>): PriorityQueue<Edge3d> {
        val minHeap = PriorityQueue<Edge3d>(compareBy { it.length })
        coordinates.forEach { from ->
            coordinates.forEach { to ->
                if (from != to)
                    minHeap.add(Edge3d(from, to))
            }
        }

        return minHeap
    }


    private fun getCoordinates3d(lines: List<String>): List<Coordinate3d> =
        lines.map {
            val (x, y, z) = it.split(",").map { coord -> coord.toInt()}
            Coordinate3d(x, y, z)
        }
}