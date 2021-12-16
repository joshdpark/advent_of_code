function complete_line(right, left, line)
    return [right[left .== x][1] for x = line]
end
function check_line(line)
    pointer = 1
    while pointer < length(line)
        value = line[pointer]
        if (value in left)
            match = right[left .== value][1]
            ahead = line[pointer + 1]
            if (ahead == match)
                deleteat!(line, pointer:pointer+1)
                pointer = 1
            else
                pointer += 1
            end
        else
            pointer += 1
        end
    end
    if !all([x in left for x = line])
        return line[findfirst(x -> x in right, line)]
    else
        return reverse(complete_line(right, left, line))
    end
end

function part_a(path)
    input = readlines(path)
    left = ["(", "{", "<", "["]
    right = [")", "}", ">", "]"]
    error_scores = Dict(nothing => 0, ")" => 3, "]" => 57, "}" => 1197, ">" => 25137)
    lines = split.(input, "")
    syntax_errors = check_line.(lines)
    correct_lines = syntax_check[[typeof(x) == SubString{String} for x = syntax_check]]
    return sum([error_scores[x] for x = correct_lines])
end

@assert part_a("test") == 26397
part_a("input")

function part_b(path)
    input = readlines(path)
    left = ["(", "{", "<", "["]
    right = [")", "}", ">", "]"]
    tool_scores = Dict(")" => 1, "]" => 2, "}" => 3, ">" => 4)
    lines = split.(input, "")
    syntax_check = check_line.(lines)
    correct_lines = syntax_check[[typeof(x) == Vector{String} for x = syntax_check]]
    function get_score(line)
        s = 0
        for (index, value) in enumerate(line)
            s = s * 5 + tool_scores[value]
        end
        return s
    end
    scores = get_score.(correct_lines)
    return sort(scores)[div(length(scores), 2) + 1]
end
@assert part_b("test") == 288957
part_b("input")

"{([(<[}>{[]{[(<()>"
