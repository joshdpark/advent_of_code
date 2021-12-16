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
            println(s)
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
adj = read_parse("test1")
adj2 = read_parse("test2")

function iscap(s::String)
    cap = r"[A-Z]+"
    occursin(cap, s)
end

adj_t = Dict("start" => ["A", "b"], "A" => ["end"], "b" => ["end"])

function traverse(start::String, adjacency_list::Dict{String, Vector{String}}, path = String[], paths = Vector{String}[], path_attempt = Int(1))
    connections = adjacency_list[start]
    for c in connections
        # print("=> $c ")
        if c == "end"
            println("end")
            push!(path, c)
            push!(paths, path)
            # return
            empty!(path)
            # return paths
        end
        if (c in path) & !iscap(c)
            # println("deadend")
            continue
        end
        if !iscap(c) & !iscap(start) & !isequal(start, "start")
            # println("stop it's a deadend")
            continue
        end
        push!(path, c)
        println(path)
        traverse(c, adjacency_list, path, paths)
    end
end
traverse("start", adj)

traverse("start", adj)
traverse("A", adj)
traverse("c", adj)

# traverse("start", adj2)
