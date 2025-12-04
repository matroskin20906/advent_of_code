package org.example.stuff

enum class CompassDirection(val value: Coordinate) {
    N(Coordinate(-1, 0)),
    S(Coordinate(1, 0)),
    E(Coordinate(0, 1)),
    W(Coordinate(0, -1)),
    NE(Coordinate(-1, 1)),
    SE(Coordinate(1, 1)),
    SW(Coordinate(1, -1)),
    NW(Coordinate(-1, -1))
}