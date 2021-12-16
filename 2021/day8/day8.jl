# part a
# first figure out which codes correspond to 2 3 4 and 7 (1, 6, 4, and 8 respectively)
function part_a(path)
    input = readlines(path)
    easy = 0
    for l = input
        input = split(split(l, "|")[1])
        output = split(split(l, "|")[2])
        for o = output
            if length(o) in [2, 3, 4, 7]
                easy += 1
            end
        end
    end
    return easy
end
@assert part_a("test") == 26
part_a("input")

# part b
function part_b()
    line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    function decode(line)
        input = split(line, "|")[1]
        output = split(line, "|")[2]
        sets = Set.(split(input))
        _zero = _one = _two = _three = _four = _five = _six = _seven = _eight = AbstractSet{}
        _one = sets[findfirst(f -> length(f) == 2, sets)]
        _seven = sets[findfirst(f -> length(f) == 3, sets)]
        _four = sets[findfirst(f -> length(f) == 4, sets)]
        _eight = sets[findfirst(f -> length(f) == 7, sets)]
        sixes = sets[findall(f -> length(f) == 6, sets)]
        fives = sets[findall(f -> length(f) == 5, sets)]
        for s = sixes
            if !issubset(_one, s)
                _six = s
            elseif !issubset(_four, s)
                _zero = s
            else
                _nine = s
            end
        end
        for f = fives
            if issubset(_one, f)
                _three = f
            end
        end
        a = setdiff(_seven, _one)
        c = setdiff(_eight, _six)
        f = setdiff(_one, c)
        d = setdiff(_eight, _zero)
        e = setdiff(_eight, _nine)
        b = setdiff(_four, union(f,c,d))
        g = setdiff(_eight, union(a, b, c, d, e, f))
        _two = union(a, c, d, e, g)
        _five = union(a, b, d, f, g)
        _nine = union(a, b, c, d, f, g)
        results = Int[]
        for i = split(output)
            i = Set(i)
            if i == _zero
                push!(results, 0)
            elseif i == _one
                push!(results, 1)
            elseif i == _two
                push!(results, 2)
            elseif i == _three
                push!(results, 3)
            elseif i == _four
                push!(results, 4)
            elseif i == _five
                push!(results, 5)
            elseif i == _six
                push!(results, 6)
            elseif i == _seven
                push!(results, 7)
            elseif i == _eight
                push!(results, 8)
            elseif i == _nine
                push!(results, 9)
            end
        end
        return results
    end
    decode(line)
    input = readlines("input")
    output = Int[]
    for l in input
        push!(output, parse(Int, join(string.(decode(l)))))
    end
    return sum(output)
end
part_b()
