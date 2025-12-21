from z3 import *

def solve(equations):
    names = sorted({v for vars_, _ in equations for v in vars_})
    vars = {name: Int(name) for name in names}

    opt = Optimize()

    for v in vars.values():
        opt.add(v >= 0)
    for vars_, value in equations:
        opt.add(sum(vars[v] for v in vars_) == value)

    total = sum(vars.values())
    opt.minimize(total)
    assert opt.check() == sat
    model = opt.model()

    return model.evaluate(total)

equations = [
    # Output from the Swift code goes here
]

n = 0
for eq in equations:
    result = solve(eq)
    n += result.as_long()
print(n)
