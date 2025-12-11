package org.example.solution

import kotlin.collections.forEach
import kotlin.collections.set

class Day11 : Solution {
    companion object {
        private const val START_1 = "you"
        private const val START_2 = "svr"
        private const val FINISH = "out"
        private const val DIGITAL_TO_ANALOG_CONVERTER = "dac"
        private const val FAST_FOURIER_TRANSPORT = "fft"
    }

    override fun partOne(lines: List<String>): String {
        val neighbours = makeNeighbours(lines)
        return calculateWayCount(
            topologySort(neighbours.keys.toMutableList(), neighbours),
            neighbours,
            START_1,
            FINISH
        ).toString()
    }

    override fun partTwo(lines: List<String>): String {
        val neighbours = makeNeighbours(lines)
        val vertexes = neighbours.keys.toMutableList()

        val sortedVertexes = topologySort(vertexes, neighbours)
        var first = DIGITAL_TO_ANALOG_CONVERTER
        var second = FAST_FOURIER_TRANSPORT
        if (sortedVertexes.indexOf(FAST_FOURIER_TRANSPORT) < sortedVertexes.indexOf(DIGITAL_TO_ANALOG_CONVERTER)) {
            first = FAST_FOURIER_TRANSPORT
            second = DIGITAL_TO_ANALOG_CONVERTER
        }
        val ways: Long = calculateWayCount(sortedVertexes, neighbours, START_2, first).toLong() *
                calculateWayCount(sortedVertexes, neighbours, first, second) *
                calculateWayCount(sortedVertexes, neighbours, second, FINISH)
        return ways.toString()
    }

    private fun calculateWayCount(sortedVertexes: List<String>, neighbours: Map<String, List<String>>, from: String, to: String): Int {
        val paths = mutableMapOf<String, Int>()
        paths[from] = 1
        sortedVertexes.forEach {
            neighbours[it]!!.forEach { neighbour ->
                paths[neighbour] = paths.getOrDefault(neighbour, 0) + paths.getOrDefault(it, 0)
            }
        }

        return paths[to]!!
    }

    private fun topologySort(vertexes: List<String>, neighbours: Map<String, List<String>>): List<String> {
        var revertNeighbours = mutableMapOf<String, MutableList<String>>()
        neighbours.forEach {
            it.value.forEach { vertex ->
                if (!revertNeighbours.contains(vertex))
                    revertNeighbours[vertex] = mutableListOf()
                revertNeighbours[vertex]!!.add(it.key)
            }
        }
        (vertexes.toSet() - revertNeighbours.keys).forEach {
            revertNeighbours[it] = mutableListOf()
        }

        val result = mutableSetOf<String>()
        while (result.size < vertexes.size) {
            val v = revertNeighbours.entries.firstOrNull { it.value.isEmpty() && !result.contains(it.key) }!!.key
            result.add(v)
            revertNeighbours = revertNeighbours.mapValues {
                if (it.value.contains(v)) {
                    it.value.remove(v)
                }
                it.value
            }.toMutableMap()
        }

        return result.toList()
    }

    private fun makeNeighbours(lines: List<String>): Map<String, List<String>> {
        val neighbours = mutableMapOf<String, List<String>>()

        lines.forEach { line ->
            val deviceAndOutputs = line.split(":")
            val device = deviceAndOutputs[0]
            val outputs = deviceAndOutputs[1].trim().split(" ")
            neighbours[device] = outputs
        }

        neighbours[FINISH] = listOf()

        return neighbours
    }
}