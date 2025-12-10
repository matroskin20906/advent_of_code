package org.example.solution

import com.google.ortools.Loader
import com.google.ortools.linearsolver.MPSolver
import com.google.ortools.linearsolver.MPVariable
import kotlin.math.pow

data class Machine(val lightsGoal: Int, val buttons: List<Int>, val joltageRequirements: List<Int>) {
    override fun toString(): String {
        return "${lightsGoal.toString(2)}, $buttons $joltageRequirements"
    }
}

class Day10 : Solution {
    override fun partOne(lines: List<String>): String {
        val machines = getMachines(lines)
        return machines.sumOf { machine ->
            val positions = mutableSetOf<Int>()
            positions.add(0)
            var steps = 0
            val newPositions = mutableSetOf<Int>()
            while (!positions.any { it == machine.lightsGoal }) {
                newPositions.clear()
                for (position in positions) {
                    for (button in machine.buttons) {
                        newPositions.add(position xor button)
                    }
                }
                steps++
                positions.clear()
                positions.addAll(newPositions)
            }
            steps
        }.toString()
    }

    override fun partTwo(lines: List<String>): String {
        Loader.loadNativeLibraries()

        return getMachines(lines).sumOf { machine ->
            val joltageAffected = mutableMapOf<Int, MutableList<Int>>()
            for (i in machine.buttons.indices) {
                var buttonToProcess = machine.buttons[i]
                var joltageNumber = 0
                while (buttonToProcess != 0) {
                    if (buttonToProcess % 2 == 1) {
                        if (joltageNumber in joltageAffected) {
                            joltageAffected[joltageNumber]!!.add(i)
                        } else {
                            val affectedButtons = mutableListOf<Int>()
                            affectedButtons.add(i)
                            joltageAffected[joltageNumber] = affectedButtons
                        }
                    }
                    joltageNumber++
                    buttonToProcess = buttonToProcess shr 1
                }
            }

            val solver = MPSolver.createSolver("CBC")
            val variables = mutableListOf<MPVariable>()

            for (button in machine.buttons.indices) {
                variables.add(solver.makeIntVar(0.0, Double.POSITIVE_INFINITY, "x$button"))
            }

            for (i in machine.joltageRequirements.indices) {
                val c1 = solver.makeConstraint(Double.NEGATIVE_INFINITY, machine.joltageRequirements[i].toDouble())
                val c2 = solver.makeConstraint(machine.joltageRequirements[i].toDouble(), Double.POSITIVE_INFINITY)
                joltageAffected[i]!!.forEach {
                    c1.setCoefficient(variables[it], 1.0)
                    c2.setCoefficient(variables[it], 1.0)
                }
            }

            val objective = solver.objective()
            for (variable in variables) {
                objective.setCoefficient(variable, 1.0)
            }

            objective.setMinimization()
            solver.solve()

            objective.value()
        }.toString()
    }


    private fun getMachines(lines: List<String>): List<Machine> {
        val machines = mutableListOf<Machine>()
        lines.forEach {
            var lightsGoal = 0
            for (i in it.indexOf(']') -1 downTo 1) {
                lightsGoal = lightsGoal shl 1
                if (it[i] == '#')
                    lightsGoal = (lightsGoal + 1)
            }

            val buttons = mutableListOf<Int>()
            val buttonsString = it.substring(it.indexOf(']') + 2, it.indexOf('{') - 1)
            buttonsString.split(" ").forEach { buttonRow ->
                var button = 0
                buttonRow.substring(1, buttonRow.length - 1).split(",")
                    .forEach { action -> button += 2.0.pow(action.toDouble()).toInt()}
                buttons.add(button)
            }

            val joltageRequirements = mutableListOf<Int>()
            it.substring(it.indexOf('{') + 1, it.indexOf('}')).split(",").forEach { joltageRequirement ->
                joltageRequirements.add(joltageRequirement.toInt())
            }

            machines.add(Machine(lightsGoal, buttons, joltageRequirements))
        }

        return machines
    }
}