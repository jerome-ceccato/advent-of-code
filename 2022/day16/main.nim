import std/re
import std/sugar
import std/sequtils
import std/tables
import std/strformat
import strutils

# Types
type Valve = ref object
  name: string
  pressure: int
  linked: seq[Valve]

type Network = Table[string, Valve]

type Simulation = object
  valve: Valve
  duration: int
  score: int

# Input
proc buildValveNetwork(filename: string): Network =
  let parsed = collect(newSeq):
    for line in lines(filename):
      var results = newSeq[string](3)
      discard find(line, re"Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? ([\w\w,? ?]+)", matches = results)
      results

  var valves = initTable[string, Valve]()
  for raw in parsed:
    valves[raw[0]] = Valve(name: raw[0], pressure: parseInt(raw[1]), linked: @[])
  for raw in parsed:
    valves[raw[0]].linked = raw[2].split(", ").map((n) => valves[n])
  return valves

# Solution
proc distance(self: Network, origin: Valve, target: Valve): int =
  const unreachable = 9999999
  var steps = 0
  var visited = @[origin.name]
  var queue = @[origin]

  while queue.len > 0 and not queue.contains(target):
    var next = newSeq[Valve]()
    for node in queue:
      for neighbor in node.linked:
        if not visited.contains(neighbor.name):
          visited.add(neighbor.name)
          next.add(neighbor)
    queue = next
    steps += 1
  return if queue.len > 0: steps else: unreachable


proc totalPressureReleased(self: Network, current: Valve, target: Valve, time: int): Simulation =
  let duration = self.distance(current, target) + 1

  if duration > time:
    return Simulation(valve: target, duration: 0, score: 0) # Can't reach and open the valve
  return Simulation(valve: target, duration: duration, score: (time - duration) * target.pressure)

proc openValves(self: Network, current: Valve, closed: seq[Valve], time: int): int =
  if time <= 0 or closed.len == 0:
    return 0

  let simulations = closed.map((v) => self.totalPressureReleased(current, v, time))
  let best = simulations[simulations.map((s) => s.score).maxIndex]

  echo &"Moving to {best.valve.name}: score({best.score}) -> time({best.duration})"

  return best.score + self.openValves(best.valve, closed.filter((v) => v != best.valve), time - best.duration)


# Main
let network = buildValveNetwork("input")
echo network.openValves(network["AA"], toSeq(network.values).filter((v) => v.pressure > 0), 30)
