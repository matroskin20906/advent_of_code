package org.example.stuff

data class Edge3d(val fromCoordinate3d: Coordinate3d, val toCoordinate3d: Coordinate3d) {
    val length : Double = fromCoordinate3d.distance(toCoordinate3d)
}