using BenchmarkTools

function read_parse(path)
    """
    The output will be 2 arrays, the first will be an array of all possible points.
    The second array will be indexed as the first one but will contain all of the possible connections.
    """
    lines = readlines(path)
    adj = Dict{String, Vector{String}}()
    # get a list of possible caves
    for line in lines
        l = split(line, "-")
        for s in l
            get!(adj, s, String[])
        end
    end
    for line in lines
        l = split(line, "-")
        if l[2] != "start"
            push!(adj[l[1]], l[2])
        end
        if l[1] != "start"
            push!(adj[l[2]], l[1])
        end
    end
    return adj
end

function iscap(s::String)
    cap = r"[A-Z]+"
    occursin(cap, s)
end
function iscap(s::Char)
    cap = r"[A-Z]+"
    occursin(cap, string(s))
end

function constraint(path::Vector{String})
    path = path[path .!= "start"]
    # get all lowercase in path
    lower = path[findall(!iscap, path)]
    # in lower is there at least one element that shows up twice?
    return unique(lower) != lower
end

function dfs_paths(adjacency_list, source, goal, stack = [(source, [source])]; partb = false)
    unique_paths = 0
    while !isempty(stack)
        vertex, path = pop!(stack)
        for a in adjacency_list[vertex]
            if partb && constraint(path)
                (a in path) & !iscap(a) && continue
            end
            a == "start" && continue
            if a == goal
                unique_paths += 1
            else
                push!(stack, (a, [path; [a]]))
            end
        end
    end
    return unique_paths
end

function part_a(path)
    adj = read_parse(path)
    return dfs_paths(adj, "start", "end")
end

function part_b(path)
    adj = read_parse(path)
    return dfs_paths(adj, "start", "end", partb=true)
end

@assert part_a("test1") == 10
@assert part_a("test2") == 19
@assert part_a("test3") == 226
@btime part_a("input")

part_b("test1")
part_b("test2")
part_b("test3")
@btime part_b("input")
