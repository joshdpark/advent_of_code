#day14

# approach: store each pair of letters in a list, the order doesn't matter
# for each pair check dictionary if key exists
# if it doesn't exist, create a key
# if it does exist, then parse the key to get 2 more pairs
# eg. NN -> C so NN => NC; CN
#
# NNCB
#
# first put all the doubles into a dictionary with the counts of them

function read_parse(io::IO)
    polymer = readline(io)
    readline(io) # skip an empty line
    dict  = readlines(io) # "CH -> B" => Dict("CH" => "B")
    template = Dict(x => y for (x, y) = Tuple([split(x, " -> ") for x = dict]))
    return polymer, template
end

function pry(x::String)
    template = Dict{String, Int}()
    knit = [x[i] * x[i+1] for i = 1:length(x)-1]
    [insert_dict(template, k, 1) for k = knit]
    return template
end

function insert_dict(dict::Dict, index::Union{Char, String}, value::Int)
    if !haskey(dict, index)
        dict[index] = value
    else
        dict[index] += value
    end
end

function grow(dict::Dict, template::Dict)
    growth = Dict{String, Int}()
    for (key, value) in copy(dict)
        # println("at key:$index")
        local ins = template[key]
        local f = first(key) * ins # first pair
        local s = ins * last(key) # second pair
        insert_dict(growth, f, value)
        insert_dict(growth, s, value)
    end
    return growth
end

function count_data(dict::Dict)
    result = Dict{Char, Int}()
    for (key, value) in dict
        insert_dict(result, first(key), value)
        insert_dict(result, last(key), value)
    end
    result
end

function solve(file::String, iter)
    polymer, template = open(read_parse, file)
    data = pry(polymer)
    iter == 0 && return data
    for _ in 1:iter
        data = grow(data, template)
    end
    counts = count_data(data)
    # the first and last Char in polymer needs to be accounted for
    counts[first(polymer)] += 1
    counts[last(polymer)] += 1
    # @show counts
    return div(maximum(values(counts)),2) - div(minimum(values(counts)),2)
end

#wrong answer: 1953
