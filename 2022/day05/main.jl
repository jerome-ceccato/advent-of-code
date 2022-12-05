function readstacks(raw)
    stackscount = 9
    stacks = [Vector{Char}() for _ = 1 : stackscount]

    for line in raw
        n = 1
        i = 2
        while i < length(line)
            if line[i] != ' '
                pushfirst!(stacks[n], line[i])
            end
            n += 1
            i += 4
        end
    end
    return stacks
end

struct Move
    amount::Int
    from::Int
    to::Int
end

function parsemove(line)
    r = match(r"move (\d+) from (\d+) to (\d+)", line)
    return Move(parse(Int, r[1]), parse(Int, r[2]), parse(Int, r[3]))
end

function applymove!(stacks, move::Move)
    for _ in 1:move.amount
        push!(stacks[move.to], pop!(stacks[move.from]))
    end
end

function topstack(stacks)
    return mapfoldl(last, (a,b) -> string(a, b), stacks)
end

open("input") do file
    lines = readlines(file)
    sep = findfirst(isequal(""), lines)
    stacks = readstacks(lines[1:sep - 2])
    
    for rawmove in view(lines, sep + 1:size(lines, 1))
        applymove!(stacks, parsemove(rawmove))
    end
    println(topstack(stacks))
end