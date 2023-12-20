import std/strutils
import std/sequtils
import std/tables
import std/deques
import std/sets
import std/math

type ModuleKind = enum
    flipflop, conjunction, broadcaster, none

type Module = ref object of RootObj
    label: string
    kind: ModuleKind
    inputs: seq[string] = @[]
    destinations: seq[string]

type Action = object
    pulse: bool
    source: string
    destination: string

type FlipFlopModule = ref object of Module
    state: bool = false

type ConjunctionModule = ref object of Module
    state: TableRef[string, bool]

type BroadcasterModule = ref object of Module

type FinalModule = ref object of Module
    done: bool = false

method linkInput(self: Module, input: string) {.base.} =
    self.inputs.add(input)

method linkInput(self: ConjunctionModule, input: string) =
    self.state[input] = false
    self.inputs.add(input)

method receive(self: Module, action: Action): seq[Action] {.base.} =
    discard

method receive(self: BroadcasterModule, action: Action): seq[Action] =
    self.destinations.mapIt(Action(pulse: action.pulse, source: self.label, destination: it))

method receive(self: FlipFlopModule, action: Action): seq[Action] =
    if action.pulse:
        @[]
    else:
        self.state = not self.state
        self.destinations.mapIt(Action(pulse: self.state, source: self.label, destination: it))

method receive(self: ConjunctionModule, action: Action): seq[Action] =
    self.state[action.source] = action.pulse
    let output = not self.state.values.toSeq.allIt(it)
    self.destinations.mapIt(Action(pulse: output, source: self.label, destination: it))

method receive(self: FinalModule, action: Action): seq[Action] =
    if not action.pulse:
        self.done = true
    @[]

method debugValue(self: Module): string {.base.} =
    "  "

method debugValue(self: FlipFlopModule): string =
    if self.state:
        "# "
    else:
        ". "

method debugValue(self: ConjunctionModule): string =
    var on = self.state.values.toSeq.countIt(it)
    var output = $on
    if output.len() == 1:
        output &= " "
    if on == self.state.len():
        output = "[]"
    output

proc makeModule(label: string, kind: ModuleKind, destinations: seq[string]): Module =
    case kind
    of flipflop: FlipFlopModule(label: label, kind: kind, destinations: destinations)
    of conjunction: ConjunctionModule(label: label, kind: kind, destinations: destinations, state: newTable[string, bool]())
    of broadcaster: BroadcasterModule(label: label, kind: kind, destinations: destinations)
    of none: Module(label: label, kind: kind, destinations: destinations)

proc parseModule(input: string): Module =
    var parts = input.split(" -> ")

    var label = parts[0]
    var kind = ModuleKind.none
    if label[0] in "%&":
        kind = if label[0] == '%': ModuleKind.flipflop else: ModuleKind.conjunction
        label = label[1..high(label)]
    elif label == "broadcaster":
        kind = ModuleKind.broadcaster
    
    makeModule(label, kind, parts[1].split(", "))

proc parseInput(): TableRef[string, Module] =
    let lines = readFile("input").splitLines()
    var network = newTable[string, Module]()
    for line in lines:
        let module = parseModule(line)
        network[module.label] = module
    
    # Create empty missing nodes
    var missing_modules: seq[Module] = @[]
    for node in network.values:
        for dest in node.destinations:
            if not network.hasKey(dest):
                missing_modules.add(Module(label: dest, kind: ModuleKind.none, destinations: @[]))
    for module in missing_modules:
        network[module.label] = module

    # Link inputs
    for input in network.values:
        for output in input.destinations:
            network[output].linkInput(input.label)
    network

proc press(network: TableRef[string, Module]): array[2, int] =
    var queue = [Action(pulse: false, source: "button", destination: "broadcaster")].toDeque
    var pulse_count = [0, 0]
    while queue.len() > 0:
        let action = queue.popFirst()
        #echo action.source, " ", (if action.pulse: "-high" else: "-low"), " -> ", action.destination
        pulse_count[ord(action.pulse)] += 1
        let next = network[action.destination].receive(action)
        for item in next:
            queue.addLast(item)
    pulse_count

proc warmUp(network: TableRef[string, Module]): int =
    var pulse_count = [0, 0]
    for n in 1..1000:
        let results = press(network)
        pulse_count[0] += results[0]
        pulse_count[1] += results[1]
    pulse_count[0] * pulse_count[1]

proc press2(network: TableRef[string, Module], step: int, period: TableRef[string, int]) =
    var queue = [Action(pulse: false, source: "button", destination: "broadcaster")].toDeque
    while queue.len() > 0:
        let action = queue.popFirst()
        if action.pulse and action.destination == network["rx"].inputs[0]:
            if not period.hasKey(action.source):
                period[action.source] = step
        let next = network[action.destination].receive(action)
        for item in next:
            queue.addLast(item)

proc startRx(network: TableRef[string, Module]): int =
    var steps = 0
    var period = newTable[string, int]()
    let target = network["rx"].inputs[0]
    let targetCount = network[target].inputs.len()
    while period.len() != targetCount:
        steps += 1
        press2(network, steps, period)
    
    period.values.toSeq().foldl(lcm(a, b))

#echo warmUp(parseInput())
echo startRx(parseInput())


proc sendPulse(network: TableRef[string, Module], pulse: Action) =
    var queue = [pulse].toDeque
    while queue.len() > 0:
        let action = queue.popFirst()
        let next = network[action.destination].receive(action)
        for item in next:
            queue.addLast(item)

# var network = parseInput()
# var state = false
# var loops = 0
# while true:
#     sendPulse(network, Action(pulse: false, source: "broadcaster", destination: "gz"))
#     loops += 1
#     if cast[ConjunctionModule](network["kl"]).state["xt"] != state:
#         state = not state
#         echo "Changed state after ", loops, "(", state, ")"
#     if loops mod 10000 == 0:
#         echo loops
    

# All 4 loops from the broadcaster are only contain `kl` and `rx` in common
# kl is a conjunction to pulse rx
# So we can analyze all 4 loops separately, find when they send a high pulse to kl, and infer the answer that way

proc getLinks(network: TableRef[string, Module], origin: string): HashSet[string] =
    var links = initHashSet[string]();
    var queue = [origin].toDeque()
    links.incl(origin)
    while queue.len() > 0:
        let current = queue.popFirst()
        for dest in network[current].destinations:
            if not links.contains(dest):
                links.incl(dest)
                queue.addLast(dest)
    links

proc dump(network: TableRef[string, Module], showNames: bool) = 
    const sorted_labels = ["kl", "gv", "xt", "gz", "mz", "ps", "kx", "ss", "lq", "tz", "qh", "vk", "gk", "lb", "gx"]
    var names = ""
    var values = ""
    for label in sorted_labels:
        let lb = if label == "broadcaster": ">>" else: label
        let value = network[label].debugValue()
        
        names &= " | " & lb
        values &= " | " & value
    if showNames:
        echo names
    echo values

# let links = getLinks(network, "gz")
# var snetwork = newTable[string, Module]()
# for label in links:
#     snetwork[label] = network[label]


# snetwork.dump(true)
# for i in 1..(2^13):
#     sendPulse(snetwork, Action(pulse: false, source: "broadcaster", destination: "gz"))
#     #snetwork.dump(false)
# snetwork.dump(true)
# for _ in 1..(2^11-1):
#     sendPulse(snetwork, Action(pulse: false, source: "broadcaster", destination: "gz"))
# snetwork.dump(false)
# echo "----------"
# for _ in 1..(2^11-2):
#     sendPulse(snetwork, Action(pulse: false, source: "broadcaster", destination: "gz"))

# sendPulse(snetwork, Action(pulse: false, source: "broadcaster", destination: "gz"))
# snetwork.dump(false)
# sendPulse(snetwork, Action(pulse: false, source: "broadcaster", destination: "gz"))
# snetwork.dump(false)
# for dest in network["broadcaster"].destinations:
#     let res = getLinks(network, dest)
#     echo res
#     for dest2 in network["broadcaster"].destinations:
#         if dest != dest2:
#             echo "intersect(", dest2, "): ", res.intersection(getLinks(network, dest2))

# higher than 3085000