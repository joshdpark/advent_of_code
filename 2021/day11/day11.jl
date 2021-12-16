"""
eg = [9 8 1
      2 4 6
      4 1 3]
"""

# function to find neighbors
function find_neighbor(coord::CartesianIndex{2}, map::Matrix{Int})
    address = Base.product(-1:1, -1:1)
    # don't return the input coordinate
    index_list = vec([coord + CartesianIndex(x) for x = address if x != (0,0)])
    neighbor_check = [checkbounds(Bool, field, x) for x = index_list]
    return index_list[neighbor_check]
end
function part_a(path)
    raw = readlines(path)
    input = [parse.(Int, x) for x = split.(raw, "")]
    field = reshape(reduce(vcat, input), (length(input), length(input[1])))
    flash_counter = 0
    for step in 1:100
        field .+= 1
        while any(field .> 9)
            primed = findfirst(x -> x > 9, field)
            neighbors = find_neighbor(primed, field)
            for n = neighbors
                if field[n] != 0 field[n] += 1 end
            end
            flash_counter += 1
            field[primed] = 0
        end
    end
    return flash_counter
end

@assert part_a("test") == 1656
part_a("input")

function part_b(path)
    raw = readlines(path)
    input = [parse.(Int, x) for x = split.(raw, "")]
    field = reshape(reduce(vcat, input), (length(input), length(input[1])))
    flash_counter = 0
    step = 0
    while any(field .!= 0)
        step += 1
        field .+= 1
        while any(field .> 9)
            primed = findfirst(x -> x > 9, field)
            neighbors = find_neighbor(primed, field)
            for n = neighbors
                if field[n] != 0 field[n] += 1 end
            end
            flash_counter += 1
            field[primed] = 0
        end
    end
    return step
end
@assert part_b("test") == 195
part_b("input")
