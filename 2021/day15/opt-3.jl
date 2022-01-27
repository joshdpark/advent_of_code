# Heap struct and methods
struct Heap
    items::Vector{Pair}
end
Heap() = Heap([])
Base.getindex(h::Heap, i::Int64) = Base.getindex(h.items, i)
Base.setindex!(h::Heap, item::Pair, i::Integer) = h.items[i] = item
Base.length(h::Heap) = Base.length(h.items)

function bubbleup!(h::Heap, index::Integer=length(h))
    n = length(h)
    while n > 1 && last(h[n ÷ 2]) > last(h[n])
        h[n ÷ 2], h[n] = h[n], h[n ÷ 2]
        n ÷= 2
    end
end

# example
_h = Heap([Pair("a", 9), Pair("b", 7), Pair("c", 5),
           Pair("d", 6), Pair("e", 2), Pair("f", 4),
           Pair("g", 3), Pair("h", 8)])

function pushdown!(h::Heap)
    L = length(h)
    n = 1
    while true
        n′ = n
        l = 2n
        r = l + 1
        if l ≤ L && last(h[n′]) > last(h[l])
            n′ = l
        end
        if r ≤ L && last(h[n′]) > last(h[r])
            n′ = r
        end
        if n != n′
            h[n], h[n′] = h[n′], h[n]
            n = n′
        else
            break
        end
    end
end

function insert!(item::Pair, h::Heap)
    push!(h.items, item)
    bubbleup!(h)
end

function top!(h::Heap)
    isempty(h.items) && error("heap is empty")
    last = pop!(h.items)
    if isempty(h.items)
        return last
    else
        firstitem, h[1] = h[1], last
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

function dijkstra(m::Matrix, start::Tuple{Int, Int})
    h = Heap()
    insert!(start => 0, h)
    W, H = size(m)
    seen = falses(W, H)
    risk = zeros(Int, W, H)
    while !seen[W, H]
        (i,j), r = top!(h)
        for (x,y) in ((1,0), (-1,0), (0,1), (0,-1))
            i′ = i + x
            j′ = j + y
            if 1 ≤ i′ ≤ W && 1 ≤ j′ ≤ H && !seen[i′, j′]
                seen[i′, j′] = true
                r′ = r + m[i′, j′]
                risk[i′, j′] = r′
                insert!((i′, j′) => r′, h)
            end
        end
    end
    return risk[W,H]
end

function parse_input(io::IO)
    reduce(hcat, [parse.(Integer, collect(line)) for line = eachline(io)])
end

tile(m::Matrix, x) = @. ((m - 1 + x) % 9) + 1
makecol(m::Matrix, range) = vcat([tile(m, x) for x = range]...)
makem(m::Matrix) = hcat([makecol(m, range(x, length = 5)) for x=0:4]...)

function partone(file)
    m = open(parse_input, file)
    dij = dijkstra(m, (1,1))
    return dij
end

function parttwo(file)
    raw = open(parse_input, file)
    m = makem(raw)
    dij = dijkstra(m, (1,1))
    return dij
end

function main(file)
    partone(file)
    parttwo(file)
end
