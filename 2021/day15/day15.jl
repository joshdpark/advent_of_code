# 1. parse input
# how do I turn the matrix into edges?
# how do I create a data structure for a weighted graph?
# 2. create a data graph structure
using Infiltrator

struct Vertex
    label::Integer
end
Base.convert(::Type{Vertex}, x::Integer) = Vertex(x) # Int->Vertex
# getindex(x::Dict{Vertex, <:Real}, i::Integer) = x[convert(Vertex, i)]
# get(x::Dict{Vertex, <:Real}, i::Integer) = getindex(x,i)

struct Edge
    origin::Vertex
    dest::Vertex
    weight::Float64
    Edge(o, d, w) = o == d ? error("No loops") : new(o, d, w)
end
Edge(x, y) = Edge(x, y, 1) # default weight = 1

struct Graph
    vertices::Vector{Vertex}
    adjacencylist::Dict{Vertex, Vector{Edge}}
end
vertices(g::Graph) = g.vertices
adjlist(g::Graph) = g.adjacencylist
# get(g::Graph, ind) = adjlist(g)[Vertex(ind)]
# getindex(g::Graph, ind) = get(g, ind)

struct Item
    element
    priority
end

# Heap struct and methods
struct Heap
    items::Vector{Item}
end
Heap() = Heap([])
Base.getindex(h::Heap, i::Int64) = Base.getindex(h.items, i)
Base.setindex!(h::Heap, item::Item, ind::Integer) = h.items[ind] = item
Base.length(h::Heap) = Base.length(h.items)
Base.max(i::Item, j::Item) = i.priority > j.priority ? i : j
getparentindex(parentindex) = ((parentindex - 1) ÷ 2) + 1
firstleafindex(h::Heap) = length(h) ÷ 2 + 1
childindex(ind::Integer) = ind > 0 ? [(2 * ind) + 1, 2 * (ind + 1)] .- 1 : error("no 0 index")# 1 indexing
childitems(ind::Integer, h::Heap) = h.items[childindex(ind)]
function highestprioritychild(ind::Integer, h::Heap)
    # potential issue if getting only 1 child
    child_i = childindex(ind)
    if last(child_i) ∉ 1:length(h)
        return (h.items[child_i[1]], child_i[1])
    end
    children = h.items[child_i]
    if children[1].priority < children[2].priority
        return (children[1], child_i[1])
    else
        return(children[2], child_i[2])
    end
end

function bubbleup!(h::Heap, index::Integer=length(h))
    current = h[index]
    while index > 1
        parentindex = getparentindex(index)
        if h[parentindex].priority > current.priority
            h[index] = h[parentindex]
            index = parentindex
        else
            break
        end
    end
    h[index] = current
end

# example
_pq = Heap([Item("a", 9), Item("b", 7), Item("c", 5),
           Item("d", 6), Item("e", 2), Item("f", 4),
           Item("g", 3), Item("h", 8)])

function pushdown!(pq::Heap, index::Integer = 1)
    current = pq[index]
    while index < firstleafindex(pq)
        # TODO: issue with highestprioritychild where only 1 child
        (child, childindex) = highestprioritychild(index, pq)
        if child.priority < current.priority
            pq[index] = pq[childindex]
            index = childindex
        else
            break
        end
    end
    pq[index] = current
end

function insert!(item::Item, h::Heap)
    push!(h.items, item)
    bubbleup!(h)
end

function top!(h::Heap)
    if isempty(h.items) error("heap is empty") end
    last = pop!(h.items)
    if isempty(h.items)
        return last
    else
        firstitem = h[1]
        h[1] = last
        pushdown!(h)
        return firstitem
    end
end

function update!(oldvalue, newpriority, h::Heap)
    index = findfirst(x -> x.element == oldvalue, h.items)
    if isnothing(index)
        insert!(Item(oldvalue, newpriority), h)
    else
        oldpriority = h[index].priority
        h[index] = Item(oldvalue, newpriority)
        if newpriority < oldpriority
            bubbleup!(h, index)
        elseif newpriority > oldpriority
            pushdown!(h, index)
        end
    end
end

function create_vertices(m::Matrix)
    map(x -> Vertex(x[1]), enumerate(m)) |> vec
end

function find_neighbors(coord::CartesianIndex{2}, m::Matrix{Int})
    address = [(1,0), (0,1), (-1,0), (0,-1)]
    # address = Base.product(-1:0, -1:0)
    # don't return the input coordinate
    neighbors = vec([coord + CartesianIndex(x) for x = address if x != (0,0)])
    inbounds = [checkbounds(Bool, m, x) for x = neighbors]
    return neighbors[inbounds]
end

function find_edgelist(i::Int64, ci, m::Matrix)#::[index, [edges]]
    neighbors = find_neighbors(ci[i], m)
    (Vertex(i) => [Edge(o, findfirst(==(n), ci), m[n]) for n = neighbors, o = i])
end

function create_adjlist(m::Matrix)
    ci = (vec ∘ CartesianIndices ∘ size)(m)
    ajdlist = map(x -> find_edgelist(x, ci, m), 1:length(m)) |> Dict
end

function dijkstra(graph::Graph, start::Vertex, goal::Vertex)
    h = Heap()
    insert!(Item(start, 0), h)
    distances = Dict(v => Inf for v in vertices(graph) if v != start)
    parents = Dict(v => Vertex(0) for v in vertices(graph))
    distances[start] = 0
    while !isempty(h.items)
        # print("h:$h\n  distances: $(filter(p -> p.second!=(Inf), distances))\n  parents: $(filter(p -> p.second!=(Vertex(0)), parents))\n\n")
        v = top!(h).element
        # @infiltrate
        if v == goal
            return (distances, parents, v)
        else
            for e in graph.adjacencylist[v]
                u = e.dest
                if distances[u] > distances[v] + e.weight
                    distances[u] = e.weight + distances[v]
                    parents[u] = v
                    update!(u, distances[u], h)
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

# addx(m::Matrix, x) = replace(mod.(m .+ x, 10), 0=>1)
tile(m::Matrix, x) = ((m .- 1 .+ x) .% 9) .+ 1
makecol(m::Matrix, range) = vcat([tile(m, x) for x = range]...)
makem(m::Matrix) = hcat([makecol(m, range(x, length = 5)) for x=0:4]...)

function partone(file)
    m = open(parse_input, file)
    vertices = create_vertices(m)
    adjlist = create_adjlist(m)
    g = Graph(vertices, adjlist)
    goal = last(g.vertices)
    dij = dijkstra(g, Vertex(1), goal)
    println("part1: $(dij[1][goal])")
end

function parttwo(file)
    raw = open(parse_input, file)
    m = makem(raw)
    vertices = create_vertices(m)
    adjlist = create_adjlist(m)
    g = Graph(vertices, adjlist)
    goal = last(g.vertices)
    dij = dijkstra(g, Vertex(1), goal)
    println("part2: $(dij[1][goal])")
end

function main(file)
    partone(file)
    parttwo(file)
end
