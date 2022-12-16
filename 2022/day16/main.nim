import std/re
import std/sugar
import std/sequtils
import std/tables
import std/strformat
import std/times
import strutils

# Types
type Valve = ref object
  name: string
  pressure: int
  linked: seq[Valve]

type DistanceMemo = ref Table[string, Table[string, int]]

type Network = ref object
  nodes: Table[string, Valve]
  memo: DistanceMemo

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
  return Network(nodes: valves, memo: DistanceMemo())


# Solution
proc distance(self: Network, origin: Valve): Table[string, int] =
  if self.memo.contains(origin.name):
    return self.memo[origin.name]

  var steps = 1
  var dist = to_table({origin.name: 0})
  var queue = @[origin]

  while queue.len > 0:
    var next = newSeq[Valve]()
    for node in queue:
      for neighbor in node.linked:
        if not dist.contains(neighbor.name):
          dist[neighbor.name] = steps
          next.add(neighbor)
    queue = next
    steps += 1
  
  self.memo[origin.name] = dist
  return dist

proc totalPressureReleased(self: Network, current: Valve, candidates: seq[Valve], time: int): seq[Simulation] =
  let distanceMap = self.distance(current)
  return candidates.map(proc (v: Valve): Simulation =
    let duration = distanceMap[v.name] + 1
    if duration > time:
      return Simulation(valve: v, duration: 0, score: 0) # Can't reach and open the valve
    return Simulation(valve: v, duration: duration, score: (time - duration) * v.pressure)
  )

proc openValves(self: Network, current: Valve, closed: seq[Valve], time: int, depth: int): int =
  if time <= 0 or closed.len == 0:
    return 0

  let simulations = self.totalPressureReleased(current, closed, time)
  let all = simulations.filter((s) => s.score > 0).map do (sim: Simulation) -> int:
    if depth < 2:
      echo &"[{depth}] Moving to {sim.valve.name}: score({sim.score}) -> time({sim.duration})"
    sim.score + self.openValves(sim.valve, closed.filter((v) => v != sim.valve), time - sim.duration, depth + 1)

  return if all.len > 0: all.max() else: 0


# Main
let time = cpuTime()
let network = buildValveNetwork("input")
echo network.openValves(network.nodes["AA"], toSeq(network.nodes.values).filter((v) => v.pressure > 0), 30, 0)
echo "Time taken: ", cpuTime() - time