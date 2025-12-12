package org.example.solution

data class Region(val width: Int, val height: Int, val shapesToPresent: List<Int>)

class Day12 : Solution {
    override fun partOne(lines: List<String>): String {
        val (figures, regions) = parseInput(lines)

        val figuresArea = figures.map {
            it.sumOf { row -> row.sum() }
        }

        return regions.count { region ->
            val area = region.width * region.height
            var neededArea = 0
            for (i in region.shapesToPresent.indices) {
                neededArea += figuresArea[i] * region.shapesToPresent[i]
            }

            neededArea <= area
        }.toString()
    }

    override fun partTwo(lines: List<String>): String {
        TODO("Not yet implemented")
    }

    private fun parseInput(lines: List<String>): Pair<List<List<List<Int>>>, List<Region>> {
        val figures = mutableListOf<MutableList<MutableList<Int>>>()
        var readFigure = false
        var figure = mutableListOf<MutableList<Int>>()
        val regions = mutableListOf<Region>()
        for (line in lines) {
            if (readFigure && line == "") {
                readFigure = false
                figures.add(figure)
                continue
            }

            if (readFigure) {
                val row = mutableListOf<Int>()
                for (symbol in line) {
                    if (symbol == '#') {
                        row.add(1)
                    } else {
                        row.add(0)
                    }
                }
                figure.add(row)
                continue
            }

            if (line.contains('x')) {
                val split = line.split(':')
                val widthAndHeight = split[0].split('x')
                regions.add(
                    Region(
                        widthAndHeight[0].toInt(),
                        widthAndHeight[1].toInt(),
                        split[1].trim().split(' ').map { it.toInt() })
                )
                continue
            }

            if (line.contains(':')) {
                readFigure = true
                figure = mutableListOf()
                continue
            }
        }

        return Pair(figures, regions)
    }
}