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

fun possiblePurchases(blueprint: Blueprint, simulation: Simulation): List<Resource> {
    // Assume it's never better to save up and buy multiple of the same robot at once
    return Resource.values().filter { resource ->
        canPurchase(blueprint, simulation, resource)
    }
}

fun earnResources(simulation: Simulation) {
    simulation.robots.forEach { (res, amount) ->
        simulation.resources.mutate(res, amount)
    }
}

fun updatedSimulationByPurchasing(blueprint: Blueprint, simulation: Simulation, resource: Resource): Simulation {
    val nextSimulation = simulation.clone()
    nextSimulation.resources.mutate(Resource.ORE, -blueprint.robotOreCost.getOrDefault(resource, 0))
    if (resource == Resource.OBSIDIAN) {
        nextSimulation.resources.mutate(Resource.CLAY, -blueprint.obsidianRobotClayCost)
    } else if (resource == Resource.GEODE) {
        nextSimulation.resources.mutate(Resource.OBSIDIAN, -blueprint.geodeRobotObsidianCost)
    }
    nextSimulation.robots.mutate(resource, 1)
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

fun resolveBlueprint(blueprint: Blueprint, simulation: Simulation, lastPossiblePurchases: List<Resource>, timeLeft: Int, memo: Memo): Int {
    if (timeLeft <= 0) {
        return simulation.resources.getOrDefault(Resource.GEODE, 0)
    }

    // Cache results
    val cacheAfter = 3 // Arbitrary limit to trade ram for cpu time
    val identifier = encode(simulation)
    if (timeLeft >= cacheAfter && memo.containsKey(identifier)) {
        val best = memo[identifier]!!
        // If there's a cached result with more time left, we can't possibly beat it
        if (best.first >= timeLeft) {
           return best.second
        }
    }

    // Calculate possible purchases, then apply earnings
    val purchases = possiblePurchases(blueprint, simulation)
    earnResources(simulation)

    var total = 0
    // If we can buy a geode making robot, there's no way to do better than that
    if (purchases.contains(Resource.GEODE)) {
        val nextSimulation = updatedSimulationByPurchasing(blueprint, simulation, Resource.GEODE)
        total = resolveBlueprint(blueprint, nextSimulation, purchases, timeLeft - 1, memo)
    } else {
        // Continue assuming we've purchased one of the possible robot
        total = purchases.maxOfOrNull {
            val nextSimulation = updatedSimulationByPurchasing(blueprint, simulation, it)
            resolveBlueprint(blueprint, nextSimulation, purchases, timeLeft - 1, memo)
        } ?: 0

        // if our purchase choice didn't change, there's no way we can do better than the last branch
        // otherwise, also try buying nothing
        if (purchases.isEmpty() || purchases != lastPossiblePurchases) {
            total = max(total, resolveBlueprint(blueprint, simulation, purchases, timeLeft - 1, memo))
        }
    }

    // Update cache
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
    return blueprints.sumOf { blueprint ->
        resolveBlueprint(blueprint, initialSimulation(), listOf(),24, mutableMapOf())
    }
}

fun geodesFromTopBlueprints(blueprints: List<Blueprint>): Int {
    return blueprints.take(3).map { blueprint ->
        resolveBlueprint(blueprint, initialSimulation(), listOf(),32, mutableMapOf())
    }.reduce { a,b -> a * b }
}

// Main

fun main() {
    val blueprints = parseInput("input")
    println("Testing ${blueprints.size} blueprints...")
    println("Part 1: ${qualityLevelFromBlueprints(blueprints)}")
    println("Part 2: ${geodesFromTopBlueprints(blueprints)}")
}
