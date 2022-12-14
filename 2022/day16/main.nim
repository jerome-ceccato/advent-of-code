import std/re
import std/sugar
import std/sequtils
import std/tables
import std/strformat
import std/algorithm
import strutils

# Types
type Valve = ref object
  name: string
  pressure: int
  linked: seq[Valve]

type DistanceMemo = ref Table[string, Table[string, int]] # Saves the distance from a node to all others
type SimulationMemo = ref Table[string, int] # Saves a full result given exactly similar conditions

type Network = ref object
  nodes: Table[string, Valve]
  memo: DistanceMemo
  simMemo: SimulationMemo

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
  return Network(nodes: valves, memo: DistanceMemo(), simMemo: SimulationMemo())


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
    sim.score + self.openValves(sim.valve, closed.filter((v) => v != sim.valve), time - sim.duration, depth + 1)

  return if all.len > 0: all.max() else: 0

proc memoId(current: array[2, Valve], closed: seq[Valve], time: array[2, int]): string =
  let closedstr = foldl(closed.map((x) => x.name), a&b, "")
  return &"{current[0].name}-{current[1].name}-{time[0]}-{time[1]}-" & closedstr 

proc openValvesWithFriendlyElephant(self: Network, current: array[2, Valve], closed: seq[Valve], time: array[2, int], depth: int): int =
  if (time[0] <= 0 and time[1] <= 0) or closed.len == 0:
    return 0

  # Try to cache simulations
  let id = memoId(current, closed, time)
  if self.simMemo.contains(id):
    return self.simMemo[id]

  # Limit the number of options to explore
  const relevantPaths = 7

  # Only keep the highest targets to eliminate useless nodes
  var mySimulations = self.totalPressureReleased(current[0], closed, time[0]).filter((s) => s.score > 0)
  var elephantSimulations = self.totalPressureReleased(current[1], closed, time[1]).filter((s) => s.score > 0)
  mySimulations.sort((x, y) => cmp(x.score, y.score), SortOrder.Descending)
  elephantSimulations.sort((x, y) => cmp(x.score, y.score), SortOrder.Descending)
  mySimulations = mySimulations[0 ..< min(mySimulations.len, relevantPaths)]
  elephantSimulations = elephantSimulations[0 ..< min(elephantSimulations.len, relevantPaths)]

  # Make sure there is a least 1 simulation, one of the two can idle while the other still has time
  if mySimulations.len == 0:
    mySimulations.add(Simulation(valve: current[0], duration: 0, score: 0))
  if elephantSimulations.len == 0:
    elephantSimulations.add(Simulation(valve: current[1], duration: 0, score: 0))

  var all = newSeq[int]()
  for m in 0 ..< mySimulations.len:
    for e in 0 ..< elephantSimulations.len:
      let me = mySimulations[m]
      let elephant = elephantSimulations[e]

      if me.valve != elephant.valve and (me.score > 0 or elephant.score > 0):
        all.add(me.score + elephant.score + self.openValvesWithFriendlyElephant(
          [me.valve, elephant.valve], 
          closed.filter do (v: Valve) -> bool: v != me.valve and v != elephant.valve,
          [time[0] - me.duration, time[1] - elephant.duration],
          depth + 1
        ))
  
  let res = if all.len > 0: all.max() else: 0
  self.simMemo[id] = res
  return res


# Main
proc run(hasElephant: bool): int =
  let time = 30
  let network = buildValveNetwork("input")
  let startNode = network.nodes["AA"]
  let candidates = toSeq(network.nodes.values).filter((v) => v.pressure > 0)

  if not hasElephant:
    return network.openValves(startNode, candidates, time, 0)
  else:
    return network.openValvesWithFriendlyElephant([startNode, startNode], candidates, [time - 4, time - 4], 0)


echo run(hasElephant = false)
echo run(hasElephant = true)
