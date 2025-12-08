package org.example.stuff

import kotlin.math.sqrt

data class Coordinate3d(val x: Int, val y: Int, val z: Int) {
    fun distance(coordinate3d: Coordinate3d) : Double {
        val deltaX = (this.x - coordinate3d.x).toDouble()
        val deltaY = (this.y - coordinate3d.y).toDouble()
        val deltaZ = (this.z - coordinate3d.z).toDouble()
        return sqrt(deltaX * deltaX + deltaY * deltaY + deltaZ * deltaZ)
    }
}