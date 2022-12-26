import math


class Point:
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

    def __add__(self, o):
        return Point(self.x + o.x, self.y + o.y)

    def __eq__(self, o) -> bool:
        return self.x == o.x and self.y == o.y

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def clamp(self, bounds):
        self.x = (self.x + bounds.x) % bounds.x
        self.y = (self.y + bounds.y) % bounds.y
        return self

    def __repr__(self):
        return "".join(["Point(", str(self.x), ",", str(self.y), ")"])


class Direction:
    @staticmethod
    def from_c(c):
        mapping = "^v<>"
        if c in mapping:
            return ["^v<>".index(c)]
        else:
            return []

    @staticmethod
    def offset(dir):
        return [Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0)][dir]


class Board:
    def __init__(self, w: int, h: int, blizzards):
        self.width = w
        self.height = h
        self.initial_blizzards = blizzards

    def update_blizzards(self, blizzards):
        bounds = Point(self.width, self.height)
        next_blizz = [[[] for _ in range(bounds.x)]
                      for _ in range(bounds.y)]

        for y in range(self.height):
            for x in range(self.width):
                for bz in blizzards[y][x]:
                    pos = (Point(x, y) + Direction.offset(bz)).clamp(bounds)
                    next_blizz[pos.y][pos.x].append(bz)
        return next_blizz

    def cache_blizzards(self):
        lcm = int(self.width * self.height / math.gcd(self.width, self.height))
        blizz = self.initial_blizzards
        all = [blizz]
        for i in range(lcm - 1):
            next = self.update_blizzards(blizz)
            all.append(next)
            blizz = next
        return all

    def possible_positions(self, blizzards, position):
        all_offsets = [Point(0, 0),
                       Point(0, -1),
                       Point(0, 1),
                       Point(-1, 0),
                       Point(1, 0)]
        candidates = [(position + offset) for offset in all_offsets]
        special_positions = [Point(0, -1), Point(self.width - 1, self.height)]
        return filter(lambda p: (p in special_positions) or (p.x >= 0 and p.x < self.width and p.y >= 0 and p.y < self.height and len(blizzards[p.y][p.x]) == 0), candidates)

    def run(self, destinations):
        round = 0
        all_boards = self.cache_blizzards()
        queue = {Point(0, -1)}

        for end in destinations:
            while not (end in queue):
                next = all_boards[(round + 1) % len(all_boards)]
                next_queue = set()
                for pos in queue:
                    next_queue.update(self.possible_positions(next, pos))
                queue = next_queue
                round += 1
            queue = {end}

        return round


def read_input():
    with open("input") as f:
        contents = f.read()
    lines = [l[1:-1] for l in contents.splitlines()[1:-1]]
    blizz = [[Direction.from_c(cell) for cell in line] for line in lines]
    return Board(len(lines[0]), len(lines), blizz)


if __name__ == '__main__':
    part1 = read_input()
    print(part1.run([Point(part1.width - 1, part1.height)]))

    part2 = read_input()
    print(part2.run([
        Point(part2.width - 1, part2.height),
        Point(0, -1),
        Point(part2.width - 1, part2.height)]))
