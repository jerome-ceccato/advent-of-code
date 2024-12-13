import functools
import re
from sympy import symbols, Eq, solve

def read_input():
    with open("input") as f:
        contents = f.read()
    blocks = contents.split("\n\n")
    data = [list(map(lambda n: int(n), re.findall("\d+", x))) for x in blocks]
    return data

# I didn't sign up for maths
def solve_once(block):
    adjustment = 10000000000000
    a, b = symbols('a b')
    eq1 = Eq(block[0] * a + block[2] * b, block[4] + adjustment)
    eq2 = Eq(block[1] * a + block[3] * b, block[5] + adjustment)
    solution = solve((eq1, eq2), (a, b))
    if solution[a].is_integer and solution[b].is_integer:
        return solution[a] * 3 + solution[b]
    return 0


solutions = [solve_once(a) for a in read_input()]
print(functools.reduce(lambda a,b: a+b, solutions))
