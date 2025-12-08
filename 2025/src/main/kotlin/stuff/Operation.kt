package org.example.stuff

enum class Operation {
    PLUS, MULT;

    fun apply(a: Long, b: Long) : Long {
        return when (this) {
            PLUS -> a + b
            MULT -> a * b
        }
    }

    fun apply(a: Int, b: Int) : Int {
        return when (this) {
            PLUS -> a + b
            MULT -> a * b
        }
    }
}