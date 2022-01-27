using DataStructures

struct Edge
    origin::Int
    dest::Int
    weight::Float64
    Edge(o, d, w) = o == d ? error("No loops") : new(o, d, w)
end
Edge(x, y) = Edge(x, y, 1) # default weight = 1

struct Graph
    vertices::Vector{Int}
    adjacencylist::Dict{Int, Vector{Edge}}
end
vertices(g::Graph) = g.vertices
adjlist(g::Graph) = g.adjacencylist

function create_vertices(m::Matrix)
    map(x -> Int(x[1]), enumerate(m)) |> vec
end

function find_neighbors(coord::CartesianIndex{2}, m::Matrix{Int})
    address = [(1,0), (0,1), (-1,0), (0,-1)]
    neighbors = vec([coord + CartesianIndex(x) for x = address if x != (0,0)])
    inbounds = [checkbounds(Bool, m, x) for x = neighbors]
    return neighbors[inbounds]
end

function find_edgelist(i::Int64, ci, m::Matrix)#::[index, [edges]]
    neighbors = find_neighbors(ci[i], m)
    (Int(i) => [Edge(o, findfirst(==(n), ci), m[n]) for n = neighbors, o = i])
end

function create_adjlist(m::Matrix)
    ci = (vec ∘ CartesianIndices ∘ size)(m)
    ajdlist = map(x -> find_edgelist(x, ci, m), 1:length(m)) |> Dict
end

function dijkstra(graph::Graph, start::Int, goal::Int)
    h = PriorityQueue()
    enqueue!(h, start, 0)
    distances = Dict(v => typemax(v) for v in vertices(graph) if v != start)
    # distances = fill(Inf, length(vertices(graph)))
    parents = Dict(v => 0 for v in vertices(graph))
    distances[start] = 0
    while !isempty(h)
        v = dequeue!(h)
        v == goal && return (distances, v)
        for e in graph.adjacencylist[v]
            u = e.dest
            if distances[u] > distances[v] + e.weight
                distances[u] = e.weight + distances[v]
                # parents[u] = v
                enqueue!(h, u, distances[u])
            end
        end
    end
    return (nothing, distances)
end

function parse_input(io::IO)
    text = readlines(io)
    matrix = zeros(Integer, length(text), length(first(text)))
    reduce(hcat, [parse.(Integer, collect(line)) for line = text])
end

tile(m::Matrix, x) = ((m .- 1 .+ x) .% 9) .+ 1
makecol(m::Matrix, range) = vcat([tile(m, x) for x = range]...)
makem(m::Matrix) = hcat([makecol(m, range(x, length = 5)) for x=0:4]...)

function partone(file)
    m = open(parse_input, file)
    vertices = 1:length(m)
    adjlist = create_adjlist(m)
    g = Graph(vertices, adjlist)
    goal = last(g.vertices)
    dij = dijkstra(g, Int(1), goal)
    return dij[1][goal]
end

function parttwo(file)
    raw = open(parse_input, file)
    m = makem(raw)
    vertices = create_vertices(m)
    adjlist = create_adjlist(m)
    g = Graph(vertices, adjlist)
    goal = last(g.vertices)
    dij = dijkstra(g, Int(1), goal)
    return dij[1][goal]
end

function main(file)
    partone(file)
    parttwo(file)
end

function heapifyfirst!(q)
    L = length(q)
    n = 1
    while true
        n′ = n
        l = 2 * n
        r = l + 1
        if l ≤ L && first(q[n′]) > first(q[l])
            n′ = l
        end
        if r ≤ L && first(q[n′]) > first(q[r])
            n′ = r
        end
        if n != n′
            q[n], q[n′] = q[n′], q[n]
            n = n′
        else
            break
        end
    end
end

function heapifylast!(q)
    n = length(q)
    while n > 1 && first(q[n ÷ 2]) > first(q[n])
        q[n ÷ 2], q[n] = q[n], q[n ÷ 2]
        n ÷= 2
    end
end

function dij_other(input)
    W, H = size(input)
    seen = falses(W, H)
    risk = zeros(Int, W, H)
    q = [0 => (1, 1)]
    while !seen[W, H]
        r, (i, j) = q[1]
        q[1] = q[end]
        pop!(q)
        heapifyfirst!(q)
        for (δᵢ, δⱼ) in ((-1, 0), (1, 0), (0, -1), (0, 1))
            i′ = i + δᵢ
            j′ = j + δⱼ
            if 1 ≤ i′ ≤ W && 1 ≤ j′ ≤ H && !seen[i′, j′]
                seen[i′, j′] = true
                r′ = r + input[i′, j′]
                risk[i′, j′] = r′
                push!(q, r′ => (i′, j′))
                heapifylast!(q)
            end
        end
    end
    risk[W, H]
end

f(x, y) = ((x + y) % 9) + 1
