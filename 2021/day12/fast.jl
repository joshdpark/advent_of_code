big(name) = name[1] in 'A':'Z'
big_n(name) = isuppercase(name[1])

function make_graph(input)
    graph = Vector{Int}[]
    positions = Dict{String, Int}()
    big_caves = Dict{String, Vector{Int}}()
    for (key₁, key₂) in input
        for key in (key₁, key₂)
            if big(key) && !haskey(big_caves, key)
                big_caves[key] = Int[]
            end
            if !big(key) && !haskey(positions, key)
                push!(graph, Int[])
                positions[key] = length(graph)
            end
        end
        if big(key₁)
            push!(big_caves[key₁], positions[key₂])
        elseif big(key₂)
            push!(big_caves[key₂], positions[key₁])
        else
            pos₁ = positions[key₁]
            pos₂ = positions[key₂]
            push!(graph[pos₁], pos₂)
            push!(graph[pos₂], pos₁)
        end
    end
    for nodes in values(big_caves)
        for i in eachindex(nodes)
            posᵢ = nodes[i]
            push!(graph[posᵢ], posᵢ)
            for j = (i + 1):lastindex(nodes)
                println("putting $(nodes[j]) in graph to $(graph[posᵢ])")
                posⱼ = nodes[j]
                push!(graph[posᵢ], posⱼ)
                push!(graph[posⱼ], posᵢ)
            end
        end
    end
    return graph, positions["start"], positions["end"]
end

function solve1(input)
    graph, S, E = make_graph(input)
    Smask = 1 << (S - 1)
    total = 0
    counts = Dict{Tuple{Int, Int}, Int}((S, Smask) => 1)
    counts′ = Dict{Tuple{Int, Int}, Int}()
    while !isempty(counts)
        empty!(counts′)
        for ((pos, mask), count) in counts
            if pos == E
                total += count
                continue
            end
            for pos′ in graph[pos]
                mask′ = mask | (1 << (pos′ - 1))
                mask′ != mask || continue
                key′ = (pos′, mask′)
                counts′[key′] = get(counts′, key′, 0) + count
            end
        end
        counts′, counts = counts, counts′
    end
    total
end

function solve2(input)
    graph, S, E = make_graph(input)
    Smask = 1 << (S - 1)
    total = 0
    counts = Dict{Tuple{Int, Int, Int}, Int}((S, Smask, 0) => 1)
    counts′ = Dict{Tuple{Int, Int, Int}, Int}()
    while !isempty(counts)
        empty!(counts′)
        for ((pos, mask, extra), count) in counts
            if pos == E
                total += count
                continue
            end
            for pos′ in graph[pos]
                mask′ = mask | (1 << (pos′ - 1))
                extra′ = extra
                if mask′ == mask
                    if extra == 0 && pos′ != S
                        extra′ = pos′
                    else
                        continue
                    end
                end
                key′ = (pos′, mask′, extra′)
                counts′[key′] = get(counts′, key′, 0) + count
            end
        end
        counts′, counts = counts, counts′
    end
    total
end

const input = [let (s, t) = split(line, '-'); (s, t); end for line in eachline("test1")]

@btime solve1(input)
@btime solve2(input)
