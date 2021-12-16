function read_parse(path)
    input = [parse.(Int, x) for x =  split.(readlines(path), "")]
    matrix = reshape(reduce(vcat, input), (length(input[1]), size(input, 1)))
    return matrix
end
function find_neighbor(coord::CartesianIndex{2}, field::Matrix{Int})
    next_door = [(-1, 0), (1, 0), (0, 1), (0, -1)]
    neighbors = [coord + CartesianIndex(x) for x = next_door]
    return neighbors[[checkbounds(Bool, field, x) for x = neighbors]]
end
function is_lowest(coord::CartesianIndex{2}, field::Matrix{Int})
    neighbors = find_neighbor(coord, field)
    coord_value = field[coord]
    neighbor_values = field[neighbors]
    return all(coord_value .< neighbor_values)
end

function part_a(path)
    field = read_parse(path)
    low_points = CartesianIndex{2}[]
    for p in CartesianIndices(field)
        is_lowest(p, field) ? push!(low_points, p) : nothing
    end
    return sum(field[low_points] .+ 1)
end
@assert part_a("test") == 15
part_a("input")

# return nothing if there is more than one possible lower traversal path
function traverse_lowest(coord::CartesianIndex{2}, field::Matrix{Int}, paths = CartesianIndex{2}[])
    addresses = find_neighbor(coord, field)
    coord_value = field[coord]
    # is there more than one path?
    # if sum(coord_value .> field[addresses]) > 1 return end
    for address in addresses
        neighbor = field[address]
        if neighbor == 9 continue end
        if neighbor < coord_value
            push!(paths, address)
            traverse_lowest(address, field, paths)
        end
    end
    # concat the starting coord to the path
    return vcat(coord, paths)
end
traverse_lowest(CartesianIndex(2,1), field)
traverse_lowest(CartesianIndex(5,2), field)
traverse_lowest(CartesianIndex(4,2), field)
traverse_lowest(CartesianIndex(5,1), field)
traverse_lowest(CartesianIndex(6,1), field)
traverse_lowest(CartesianIndex(8,1), field)

function traverse_up(coord::CartesianIndex{2}, field::Matrix{Int}, paths = CartesianIndex{2}[])
    addresses = find_neighbor(coord, field)
    coord_value = field[coord]
    for address in addresses
        neighbor = field[address]
        # don't go up to fields with 9
        if neighbor == 9 continue end
        if neighbor > coord_value
            push!(paths, address)
            traverse_up(address, field, paths)
        end
    end
    # concat the starting coord to the path
    return vcat(coord, paths)
end

function part_b(path)
    field = read_parse(path)
    low_points = CartesianIndex{2}[]
    for p in CartesianIndices(field)
        is_lowest(p, field) ? push!(low_points, p) : nothing
    end
    basins = [length(unique(traverse_up(x, field))) for x = low_points]
    reduce(*, sort(basins, rev = true)[1:3])
end
part_b("test")
part_b("input")
