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

function applymove9000!(stacks, move::Move)
    for _ in 1:move.amount
        push!(stacks[move.to], pop!(stacks[move.from]))
    end
end

function applymove9001!(stacks, move::Move)
    n = size(stacks[move.from], 1) - move.amount + 1
    for _ in 1:move.amount
        push!(stacks[move.to], popat!(stacks[move.from], n))
    end
end

function topstack(stacks)
    return mapfoldl(last, (a,b) -> string(a, b), stacks)
end

function solve(crane)
    open("input") do file
        lines = readlines(file)
        sep = findfirst(isequal(""), lines)
        stacks = readstacks(lines[1:sep - 2])
        
        for rawmove in view(lines, sep + 1:size(lines, 1))
            crane(stacks, parsemove(rawmove))
        end
        println(topstack(stacks))
    end
end


solve(applymove9000!)
solve(applymove9001!)
