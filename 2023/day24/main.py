from decimal import *
from z3 import *

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

def solvep2(data):
    # position, velocity, the 3 dt for the 3 points considered
    px,py,pz,vx,vy,vz,t0,t1,t2 = Reals('px py pz vx vy vz t0 t1 t2')
    t = [t0, t1, t2]
    s = Solver()
    
    # Only 3 points are needed to solve the equation
    # For each coordinate, add equations to the solver stating that the researched origin + some delta time * the velocity 
    # matches the known coordinate at the same point in time
    for i in range(0, 3):
        s.add(px + t[i] * vx == data[i].pos.x + t[i] * data[i].vel.x)
        s.add(py + t[i] * vy == data[i].pos.y + t[i] * data[i].vel.y)
        s.add(pz + t[i] * vz == data[i].pos.z + t[i] * data[i].vel.z)
    
    s.check()
    model = s.model()
    print(model[px].as_long() + model[py].as_long() + model[pz].as_long())


if __name__ == '__main__':
    data = read_input()
    count_intersect_2d(data)
    solvep2(data)
  
