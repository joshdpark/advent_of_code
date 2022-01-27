using Graphs, SimpleWeightedGraphs

function find_neighbors(coord::CartesianIndex{2}, m::Matrix{Int})
    address = [(1,0), (0,1), (-1,0), (0,-1)]
    neighbors = vec([coord + CartesianIndex(x) for x = address if x != (0,0)])
    inbounds = [checkbounds(Bool, m, x) for x = neighbors]
    return neighbors[inbounds]
end

function findedges(i::Int, ci, m::Matrix)
    neighbors = find_neighbors(ci[i], m)
    [(o, findfirst(==(n), ci), m[n]) for n = neighbors, o = i]
end

function listedges(m::Matrix)
    ci = (vec ∘ CartesianIndices ∘ size)(m)
    ajdlist = vcat(map(x -> findedges(x, ci, m), 1:length(m))...)
end

function dijkstra(graph::Graph, start::Int, goal::Int)
    h = PriorityQueue()
    enqueue!(h, start, 0)
    # insert!(Item(start, 0), h)
    distances = Dict(v => Inf for v in vertices(graph) if v != start)
    parents = Dict(v => 0 for v in vertices(graph))
    distances[start] = 0
    while !isempty(h)
        # print("h:$h\n  distances: $(filter(p -> p.second!=(Inf), distances))\n  parents: $(filter(p -> p.second!=(Vertex(0)), parents))\n\n")
        # v = top!(h).element
        v = dequeue!(h)
        # @infiltrate
        if v == goal
            return (distances, parents, v)
        else
            for e in graph.adjacencylist[v]
                u = e.dest
                if distances[u] > distances[v] + e.weight
                    distances[u] = e.weight + distances[v]
                    parents[u] = v
                    enqueue!(h, u, distances[u])
                end
            end
        end
    end
    return (nothing, parents)
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
    g = SimpleWeightedGraph(length(m))
    edges = listedges(m)
    for edge in edges
        add_edge!(g, edge[1], edge[2], edge[3])
    end
    return g
    # return dijkstra_shortest_paths(g, 1)
end

function parttwo(file)
    raw = open(parse_input, file)
    m = makem(raw)
    vertices = create_vertices(m)
    adjlist = create_adjlist(m)
    g = Graph(vertices, adjlist)
    goal = last(g.vertices)
    dij = dijkstra(g, Vertex(1), goal)
    return dij[1][goal]
end

function main(file)
    partone(file)
    parttwo(file)
end
