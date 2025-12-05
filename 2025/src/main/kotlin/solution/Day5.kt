package org.example.solution

import kotlin.math.max

class Day5 : Solution {
    override fun partOne(lines: List<String>): String {
        val freshIdsRanges: MutableSet<Pair<Long, Long>> = mutableSetOf()
        val splitIndex = lines.indexOf("")
        val freshIdsLines = lines.subList(0, splitIndex)
        val ingredientsIds = lines.subList(splitIndex + 1, lines.size)

        freshIdsLines.forEach {
            val splitLine = it.split("-")
            freshIdsRanges.add(Pair(splitLine[0].toLong(), splitLine[1].toLong()))
        }

        val gluedRanges = glueRanges(freshIdsRanges)

        val freshIngredientsCount = ingredientsIds.count { ingredientId ->
            val id = ingredientId.toLong()

            gluedRanges.any { (from, to) ->
                when {
                    id in from..to -> true
                    from > id -> false
                    else -> false
                }
            }
        }

        return freshIngredientsCount.toString()
    }

    override fun partTwo(lines: List<String>): String {
        val freshIdsRanges: MutableList<Pair<Long, Long>> = mutableListOf()
        val splitIndex = lines.indexOf("")
        val freshIdsLines = lines.subList(0, splitIndex)
        freshIdsLines.forEach {
            val splitLine = it.split("-")
            freshIdsRanges.add(Pair(splitLine[0].toLong(), splitLine[1].toLong()))
        }

        val gluedRanges = glueRanges(freshIdsRanges)

        val freshIngredientsCount = gluedRanges.sumOf { (from, to) -> (to - from) + 1 }

        return freshIngredientsCount.toString()
    }

    private fun glueRanges(freshIdsRanges: Iterable<Pair<Long, Long>>): List<Pair<Long, Long>> {
        val sortedRanges = freshIdsRanges.sortedBy { it.first }
        val gluedRanges = mutableListOf<Pair<Long, Long>>()

        gluedRanges.add(sortedRanges.first())
        for (i in 1..<sortedRanges.size) {
            var last = gluedRanges.removeLast()
            val current = sortedRanges[i]

            if (current.first <= last.second) {
                last = Pair(last.first, max(last.second, current.second))
                gluedRanges.add(last)
            } else {
                gluedRanges.add(last)
                gluedRanges.add(current)
            }
        }
        return gluedRanges
    }
}