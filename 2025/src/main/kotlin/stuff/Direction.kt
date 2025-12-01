package org.example.stuff

enum class Direction {
    LEFT,
    RIGHT;

    companion object {
        fun fromChar(char: Char): Direction {
            return when (char) {
                'L' -> LEFT
                'R' -> RIGHT
                else -> RIGHT
            }
        }
    }
}