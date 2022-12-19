import java.io.File
import java.lang.Integer.max

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

typealias Memo = MutableMap<String, Int>

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

fun encode(simulation: Simulation, timeLeft: Int): String {
    val res = StringBuilder()
    Resource.values().forEach { resource ->
        res.append(simulation.resources.getOrDefault(resource, 0))
        res.append("-")
        res.append(simulation.robots.getOrDefault(resource, 0))
        res.append("-")
    }
    res.append(timeLeft)
    return res.toString()
}

// Finds the max amount of geode possible to obtain with a given blueprint
fun resolveBlueprint(blueprint: Blueprint, simulation: Simulation, timeLeft: Int, memo: Memo): Int {
    if (timeLeft <= 0) {
        return simulation.resources.getOrDefault(Resource.GEODE, 0)
    }

    // Cache results
    val identifier = encode(simulation, timeLeft)
    if (memo.containsKey(identifier)) {
        return memo[identifier]!!
    }

    // If we can buy a geode making robot, there's no way to do better than that
    if (canPurchase(blueprint, simulation, Resource.GEODE)) {
        earnResources(simulation)
        val nextSimulation = updatedSimulationByPurchasing(blueprint, simulation, listOf(Resource.GEODE))
        val total = resolveBlueprint(blueprint, nextSimulation, timeLeft - 1, memo)
        memo[identifier] = total
        return total
    }

    // Calculate possible purchases, then apply earnings
    val purchases = possiblePurchases(blueprint, simulation)
    earnResources(simulation)

    // Continue assuming we've purchased one of the possible robot
    var total = purchases.maxOfOrNull {
        val nextSimulation = updatedSimulationByPurchasing(blueprint, simulation, listOf(it))
        resolveBlueprint(blueprint, nextSimulation, timeLeft - 1, memo)
    } ?: 0

    // if no purchases and no stockpile, continue by not purchasing anything
    if (purchases.size < 4) {
        total = max(total, resolveBlueprint(blueprint, simulation, timeLeft - 1, memo))
    }

    memo[identifier] = total
    return total
}

fun qualityLevelFromBlueprints(blueprints: List<Blueprint>): Int {
    return blueprints.map { blueprint ->
        val quality = resolveBlueprint(blueprint, initialSimulation(), 24, mutableMapOf())
        println("Blueprint ${blueprint.id}: $quality")
        return@map quality * blueprint.id
    }.sum()
}

// Main

fun main() {
    val blueprints = parseInput("input")
    println("Testing ${blueprints.size} blueprints...")
    println("Total: ${qualityLevelFromBlueprints(blueprints)}")
}