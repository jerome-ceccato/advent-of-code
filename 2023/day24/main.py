from decimal import *

class Vector3:
    def __init__(self, x: Decimal, y: Decimal, z: Decimal):
        self.x = x
        self.y = y
        self.z = z
    
    def __repr__(self):
            return "".join(["(", str(self.x), ", ", str(self.y), ", ", str(self.z), ")"])

class Hailstone:
    def __init__(self, pos: Vector3, vel: Vector3):
        self.pos = pos
        self.vel = vel

    def __repr__(self):
        return "".join(["Hailstone(", str(self.pos), " @ ", str(self.vel), ")"])

def read_input():
    with open("input") as f:
        contents = f.read()
    lines = contents.strip(" \n").split("\n")
    data = []
    for line in lines:
        parts = line.split("@")
        pos = [Decimal(n.strip(" ")) for n in parts[0].split(",")]
        vel = [Decimal(n.strip(" ")) for n in parts[1].split(",")]
        data.append(Hailstone(Vector3(pos[0], pos[1], pos[2]), Vector3(vel[0], vel[1], vel[2])))
    return data

def line_intersects_2d(a: Hailstone, b: Hailstone):
    denom = b.vel.y * a.vel.x - b.vel.x * a.vel.y
    if denom == 0:
        return None
    v = Vector3(a.pos.x - b.pos.x, a.pos.y - b.pos.y, 0)
    t = (b.vel.x * v.y - b.vel.y * v.x) / denom
    return Vector3(a.pos.x + t * a.vel.x, a.pos.y + t * a.vel.y, 0)

def count_intersect_2d(data):
    test_area = [Decimal(200000000000000), Decimal(200000000000000), Decimal(400000000000000), Decimal(400000000000000)]
    total = 0
    for i in range(len(data)):
        for j in range(i + 1, len(data)):
            intersect = line_intersects_2d(data[i], data[j])
            is_forward = intersect is not None \
                and ((intersect.x - data[i].pos.x) / data[i].vel.x) > 0 \
                and ((intersect.x - data[j].pos.x) / data[j].vel.x) > 0 \
                and ((intersect.y - data[i].pos.y) / data[i].vel.y) > 0 \
                and ((intersect.y - data[j].pos.y) / data[j].vel.y) > 0
            if is_forward:
                if intersect.x >= test_area[0] and intersect.y >= test_area[1] and intersect.x <= test_area[2] and intersect.y <= test_area[3]:
                    total += 1
    print(total)

if __name__ == '__main__':
    data = read_input()
    count_intersect_2d(data)
  
