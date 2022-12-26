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
        return filter(lambda p: p.x >= 0 and p.x < self.width and p.y >= 0 and p.y < self.height and len(blizzards[p.y][p.x]) == 0, candidates)

    def run(self):
        round = 0
        start = Point(0, -1)
        end = Point(self.width - 1, self.height - 1)
        all_boards = self.cache_blizzards()
        queue = {start}

        print(f"Cached {len(all_boards)} boards")
        while not (end in queue) and not len(queue) == 0:
            print(f"Round {round}, queue size: {len(queue)}")
            next = all_boards[(round + 1) % len(all_boards)]
            next_queue = set()
            for pos in queue:
                next_queue.update(self.possible_positions(next, pos))
            next_queue.add(start)
            queue = next_queue
            round += 1

        if len(queue) == 0:
            return -1
        return round + 1


def read_input():
    with open("input") as f:
        contents = f.read()
    lines = [l[1:-1] for l in contents.splitlines()[1:-1]]
    blizz = [[Direction.from_c(cell) for cell in line] for line in lines]
    return Board(len(lines[0]), len(lines), blizz)


if __name__ == '__main__':
    board = read_input()
    print(f"w:{board.width}, h:{board.height}")
    # for c in board.cache_blizzards():
    #    print(c)
    # print(board.initial_blizzards)
    # print(board.update_blizzards(board.initial_blizzards))
    # print(board.cache_blizzards())
    print(board.run())
