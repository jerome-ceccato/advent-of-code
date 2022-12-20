import java.io.File
import java.lang.Integer.max
import kotlin.system.exitProcess
import kotlin.system.measureTimeMillis
import kotlin.time.ExperimentalTime
import kotlin.time.measureTime

// Utils

// Alternative to Swift's dict[x, default: 0] += y
fun MutableMap<Resource,Int>.mutate(resource: Resource, offset: Int) {
    this[resource] = this.getOrDefault(resource, 0) + offset
}

// Data

enum class Resource {
    ORE, CLAY, OBSIDIAN, GEODE
}

data class Blueprint(
    val id: Int,
    val robotOreCost: Map<Resource, Int>,
    val obsidianRobotClayCost: Int,
    val geodeRobotObsidianCost: Int
)

data class Simulation(
    val resources: MutableMap<Resource, Int>,
    val robots: MutableMap<Resource, Int>
) : Cloneable {
    public override fun clone() = Simulation(resources.toMutableMap(), robots.toMutableMap())
}

typealias Memo = MutableMap<String, Pair<Int, Int>>

fun initialSimulation(): Simulation {
    return Simulation(mutableMapOf(), mutableMapOf(Resource.ORE to 1))
}

// Parsing

fun parseInput(filename: String): List<Blueprint> {
    val pattern = Regex("Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.")
    return File(filename).useLines { lines ->
        lines.map { line ->
            val values = pattern.matchEntire(line)?.groupValues?.drop(1)?.map { it.toInt() } ?: listOf()
            Blueprint(values[0], mapOf(Resource.ORE to values[1], Resource.CLAY to values[2], Resource.OBSIDIAN to values[3], Resource.GEODE to values[5]), values[4], values[6])
        }.toList()
    }
}

// Simulation

fun canPurchase(blueprint: Blueprint, simulation: Simulation, robot: Resource): Boolean {
    var canAffordSpecialResources = true
    if (robot == Resource.OBSIDIAN) {
        canAffordSpecialResources = simulation.resources.getOrDefault(Resource.CLAY, 0) >= blueprint.obsidianRobotClayCost
    } else if (robot == Resource.GEODE) {
        canAffordSpecialResources = simulation.resources.getOrDefault(Resource.OBSIDIAN, 0) >= blueprint.geodeRobotObsidianCost
    }
    return canAffordSpecialResources && (simulation.resources.getOrDefault(Resource.ORE, 0) >= blueprint.robotOreCost.getOrDefault(robot, 0))
}

// Returns a list of all possible purchases (can be multiple at a time)
/*
fun possibleActions(blueprint: Blueprint, simulation: Simulation): List<List<Resource>> {
    val possibilities = Resource.values().flatMap { resource ->
        if (canPurchase(blueprint, simulation, resource)) {
            val nextSimulation = updatedSimulationByPurchasing(blueprint, simulation, listOf(resource))
            return@flatMap possibleActions(blueprint, nextSimulation).map {
                val copy = it.toMutableList()
                copy.add(0, resource)
                copy
            }
        } else {
            return@flatMap listOf()
        }
    }.toMutableList()

    // Always possible to do nothing
    possibilities.add(mutableListOf())
    return possibilities
}
*/
// Assume it's never better to save up and buy multiple of the same robot at once
fun possiblePurchases(blueprint: Blueprint, simulation: Simulation): List<Resource> {
    return Resource.values().filter { resource ->
        canPurchase(blueprint, simulation, resource)
    }
}

// Get resources from all robots
fun earnResources(simulation: Simulation) {
    simulation.robots.forEach { (res, amount) ->
        simulation.resources.mutate(res, amount)
    }
}

// Buys all robots in the purchase list
fun updatedSimulationByPurchasing(blueprint: Blueprint, simulation: Simulation, purchases: List<Resource>): Simulation {
    val nextSimulation = simulation.clone()
    purchases.forEach { resource ->
        nextSimulation.resources.mutate(Resource.ORE, -blueprint.robotOreCost.getOrDefault(resource, 0))
        if (resource == Resource.OBSIDIAN) {
            nextSimulation.resources.mutate(Resource.CLAY, -blueprint.obsidianRobotClayCost)
        } else if (resource == Resource.GEODE) {
            nextSimulation.resources.mutate(Resource.OBSIDIAN, -blueprint.geodeRobotObsidianCost)
        }
        nextSimulation.robots.mutate(resource, 1)
    }
    return nextSimulation
}

fun encode(simulation: Simulation): String {
    val res = StringBuilder()
    Resource.values().forEach { resource ->
        res.append(simulation.resources.getOrDefault(resource, 0))
        res.append("-")
        res.append(simulation.robots.getOrDefault(resource, 0))
        res.append("-")
    }
    return res.toString()
}

// Finds the max amount of geode possible to obtain with a given blueprint
fun resolveBlueprint(blueprint: Blueprint, simulation: Simulation, lastSimulation: Simulation?, timeLeft: Int, memo: Memo): Int {
    if (timeLeft <= 0) {
        return simulation.resources.getOrDefault(Resource.GEODE, 0)
    }

    if (timeLeft > 25) {
        println("$timeLeft - ${memo.size}")
    }

    // Cache results
    val cacheAfter = 3
    val identifier = encode(simulation)
    if (timeLeft >= cacheAfter && memo.containsKey(identifier)) {
        val best = memo[identifier]!!
        // If there's a cached result with more time left, we can't possibly beat it
        if (best.first >= timeLeft) {
           return best.second
        }
    }

    // If we can buy a geode making robot, there's no way to do better than that
    if (canPurchase(blueprint, simulation, Resource.GEODE)) {
        earnResources(simulation)
        val nextSimulation = updatedSimulationByPurchasing(blueprint, simulation, listOf(Resource.GEODE))
        val total = resolveBlueprint(blueprint, nextSimulation, simulation, timeLeft - 1, memo)
        if (timeLeft >= cacheAfter) {
            if (memo.containsKey(identifier)) {
                val best = memo[identifier]!!
                // We can get the same result in less time
                if (timeLeft > best.first) {
                    memo[identifier] = Pair(timeLeft, total)
                }
            } else {
                memo[identifier] = Pair(timeLeft, total)
            }
        }
        return total


    }

    // Calculate possible purchases, then apply earnings
    val purchases = possiblePurchases(blueprint, simulation)
    earnResources(simulation)

    // Continue assuming we've purchased one of the possible robot
    var total = purchases.maxOfOrNull {
        val nextSimulation = updatedSimulationByPurchasing(blueprint, simulation, listOf(it))
        resolveBlueprint(blueprint, nextSimulation, simulation, timeLeft - 1, memo)
    } ?: 0

    // if our purchase choice didn't change, there's no way we can do better than the last branch
    // otherwise, also try buying nothing
    if ((lastSimulation == null) || (purchases != possiblePurchases(blueprint, lastSimulation))) {
        total = max(total, resolveBlueprint(blueprint, simulation, lastSimulation, timeLeft - 1, memo))
    }

    if (timeLeft >= cacheAfter) {
        if (memo.containsKey(identifier)) {
            val best = memo[identifier]!!
            // We can get the same result in less time
            if (timeLeft > best.first) {
                memo[identifier] = Pair(timeLeft, total)
            }
        } else {
            memo[identifier] = Pair(timeLeft, total)
        }
    }


    return total
}

fun qualityLevelFromBlueprints(blueprints: List<Blueprint>): Int {
    val expected = mapOf(
        1 to 0,
        2 to 1,
        3 to 0,
        4 to 0,
        5 to 1,
        6 to 2,
        7 to 2,
        8 to 11,
        9 to 0,
        10 to 12,
        11 to 0,
        12 to 0,
        13 to 0,
        14 to 0,
        15 to 0,
        16 to 3,
        17 to 0,
        18 to 2,
        19 to 3,
        20 to 5,
        21 to 2,
        22 to 2,
        23 to 4,
        24 to 1,
        25 to 9,
        26 to 12,
        27 to 3,
        28 to 0,
        29 to 4,
        30 to 0
    )

    return blueprints.map { blueprint ->
        val quality = resolveBlueprint(blueprint, initialSimulation(), null,24, mutableMapOf())
        println("Blueprint ${blueprint.id}: $quality")
        if (expected[blueprint.id]!! != quality) {
            println("Wrong answer, expected: ${expected[blueprint.id]!!}")
            exitProcess(1)
        }
        return@map quality * blueprint.id
    }.sum()
}

fun geodesFromTopBlueprints(blueprints: List<Blueprint>): Int {
    return blueprints.take(3).map { blueprint ->
        val geodes = resolveBlueprint(blueprint, initialSimulation(), null,32, mutableMapOf())
        println("Blueprint ${blueprint.id}: $geodes")
        return@map geodes
    }.reduce { a,b -> a * b }
}

// Main

@OptIn(ExperimentalTime::class)
fun main() {
    val blueprints = parseInput("input")
    val elapsed = measureTime {
        println("Testing ${blueprints.size} blueprints...")
        //println("Part 1: ${qualityLevelFromBlueprints(blueprints)}")
        println("Part 2: ${geodesFromTopBlueprints(blueprints)}")
    }
    println("Time: $elapsed")

    //Blueprint  1: 11
    //Blueprint 2: 22
    //Blueprint 3: 17
    //Part 2: 4114
}
