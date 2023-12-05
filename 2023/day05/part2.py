class Range:
    def __init__(self, low: int, high: int):
        self.low = low
        self.high = high

    def contains(self, p: int) -> bool:
        return p >= self.low and p <= self.high

    def __repr__(self):
        return "".join(["Range(", str(self.low), ", ", str(self.high), ")"])

class Convertion:
    def __init__(self, source: Range, target: int):
        self.source = source
        self.target = target

    def __repr__(self):
        return "".join(["Convertion(", str(self.source), ", ", str(self.target), ")"])


def read_input():
    with open("input") as f:
        contents = f.read()
    sections = contents.split("\n\n")
    raw_seeds = [int(n) for n in sections[0].split(":")[1].split(" ") if n]
    seeds = [Range(raw_seeds[i], raw_seeds[i] + raw_seeds[i + 1] - 1) for i in range(0, len(raw_seeds), 2)]
    
    convertion_steps = []
    for section in sections[1:]:
        section = section.split(':\n')[1]
        maps = []
        for line in section.split("\n"):
            numbers = [int(n) for n in line.split()]
            maps.append(Convertion(Range(numbers[1], numbers[1] + numbers[2] - 1), numbers[0]))
        maps.sort(key=lambda x: x.source.low)
        convertion_steps.append(maps)

    return (seeds, convertion_steps)

INF = 2**100
if __name__ == '__main__':
    (input, steps) = read_input()

    for convertion_table in steps:
        output = []

        # Fill the gaps and add low/high bounds
        if convertion_table[0].source.low > 0:
            convertion_table.insert(0, Convertion(Range(0, convertion_table[0].source.low - 1), 0))
        convertion_table.append(Convertion(Range(convertion_table[-1].source.high + 1, INF), convertion_table[-1].source.high + 1))
        for i in range(len(convertion_table) - 1, 0, -1):
            if convertion_table[i - 1].source.high + 1 < convertion_table[i].source.low:
                convertion_table.insert(i, Convertion(Range(convertion_table[i - 1].source.high + 1, convertion_table[i].source.low - 1), convertion_table[i - 1].source.high + 1))

        for current in input:
            for conv in convertion_table:
                if conv.source.contains(current.low):
                    if conv.source.contains(current.high): # fully contained, easy conversion
                        output.append(Range(current.low - conv.source.low + conv.target, current.high - conv.source.low + conv.target))
                        break
                    else: # convert as much as possible and continue
                        converted_amount = conv.source.high - current.low
                        output.append(Range(current.low - conv.source.low + conv.target, current.low - conv.source.low + conv.target + converted_amount))
                        current.low += converted_amount + 1
        input = output
    output.sort(key=lambda r: r.low)
    print(output[0].low)
