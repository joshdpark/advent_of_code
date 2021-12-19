function read_graph(path)
    """
    The output will be 2 arrays, the first will be an array of all possible points.
    The second array will be indexed as the first one but will contain all of the possible connections.
    """
    lines = readlines(path)
    values = mapreduce(x -> split(x, '-'), vcat, lines) |> unique
    hash = Dict(v => i for (i, v) = enumerate(values))
    graph = Dict{Integer, Vector{Integer}}()
    for line in lines
        l, r = Tuple(split(line, "-"))
        get!(graph, hash[l], Integer[])
        get!(graph, hash[r], Integer[])
        l != "start" && push!(graph[hash[r]], hash[l])
        r != "start" && push!(graph[hash[l]], hash[r])
    end
    return hash, graph
end
# adj_list = read_parse("test1")
# adj_list_2 = read_parse("test2")
# adj_list_3 = read_parse("test3")
# adj_list_4 = read_parse("input")

# islower(s::String) = islowercase(s[1])
# isstart(s::String) = s == "start"
# islowerandnotstart(s::String) = islower(s) && !isstart(s)

function constraint(path::Vector{Int}, small)
    # get all lowercase in path
    lower = filter(x->x in small, path)
    # in lower is there at least one element that shows up twice?
    return unique(lower) != lower
end

# A recursive function for this; but it doesn't seem to work correctly
function dfs_recursion(graph::Dict{Integer, Vector{Integer}}, small::Set, source::Integer, goal::Integer, stack = [(source, [source])]; part_b = false)
    count = 0
    vertex, path = pop!(stack)
    # println("currently in $vertex from $path")
    # println("   $vertex is connected to $(graph[vertex])")
    for a in graph[vertex]
        # println("      descending into $a")
        if a == goal
            count += 1
        elseif part_b && constraint(path, small) && (a in path) && a in small
            continue
        elseif !part_b && (a in path) && a in small
            continue
        else
            # println("           able to move foward, pushing $a to stack")
            push!(stack, (a, [path; [a]]))
            count += dfs_recursion(graph, small, a, goal, stack, part_b = part_b)
        end
    end
    return count
end

function dfs_while(adjacency_list, source, goal, stack = [(source, [source])]; part_b = false)
    unique_paths = 0
    while !isempty(stack)
        vertex, path = pop!(stack)
        for a in adjacency_list[vertex]
            if a == goal
                unique_paths += 1
            elseif part_b && constraint(path) && (a in path) && islower(a)
                continue
            elseif !part_b && (a in path) && islower(a)
                continue
            else
                push!(stack, (a, [path; [a]]))
            end
        end
    end
    return unique_paths
end

function solve(path, b = false)
    hash, graph = read_graph(path)
    # get all hash values for small caves
    small = filter(p->!in(p.first, ["start", "end"]) && islowercase(p.first[1]), hash) |> values |> Set
    dfs_recursion(graph, small, hash["start"], hash["end"], part_b = b)
end

function part_a(path)
    adj = read_parse(path)
    return dfs_paths(adj, "start", "end")
end

function part_b(path)
    adj = read_parse(path)
    return dfs_paths(adj, "start", "end", partb=true)
end

# dfs_recursion(adj_list, "start", "end")
# dfs_while(adj_list, "start", "end")
# dfs_recursion(adj_list, "start", "end", part_b = true)
# dfs_while(adj_list, "start", "end", part_b = true)

# @btime dfs_recursion(adj_list, "start", "end")
# @btime dfs_while(adj_list, "start", "end")
# @btime dfs_recursion(adj_list, "start", "end", part_b = true)
# @btime dfs_while(adj_list, "start", "end", part_b = true)

# @btime dfs_recursion(adj_list_2, "start", "end")
# @btime dfs_while(adj_list_2, "start", "end")
# @btime dfs_recursion(adj_list_2, "start", "end", part_b = true)
# @btime dfs_while(adj_list_2, "start", "end", part_b = true)

# @btime dfs_recursion(adj_list_3, "start", "end")
# @btime dfs_while(adj_list_3, "start", "end")
# @btime dfs_recursion(adj_list_3, "start", "end", part_b = true)
# @btime dfs_while(adj_list_3, "start", "end", part_b = true)

# @btime dfs_recursion(adj_list_4, "start", "end")
# @btime dfs_while(adj_list_4, "start", "end")
# @btime dfs_recursion(adj_list_4, "start", "end", part_b = true)
# @btime dfs_while(adj_list_4, "start", "end", part_b = true)

# @assert part_a("test1") == 10
# @assert part_a("test2") == 19
# @assert part_a("test3") == 226
# @btime part_a("input")

# part_b("test2")
# part_b("test3")
# @btime part_b("input")
