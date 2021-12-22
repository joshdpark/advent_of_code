function ingest(io::String)
    coords = CartesianIndex[]
    folds = Tuple{String, Integer}[]
    x = Int[]
    y = Int[]
    fold_regex = r"[xy]=[0-9]+"
    for i = eachline(io)
        if i == ""
            continue
        elseif startswith(i, "fold")
            local m = match(fold_regex, i).match
            local p = split(m, "=")
            push!(folds, (string(p[1]), parse(Int, p[2])))
        else
            local coord = parse.(Int, split(i, ",")) .+ 1
            push!(x, coord[1])
            push!(y, coord[2])
            push!(coords, CartesianIndex(Tuple(coord)))
        end
    end
    return coords, folds, (maximum(x), maximum(y))
end

function make_matrix(coords::Vector{CartesianIndex}, size::Tuple)
    mat = zeros(Int, size)
    for coord = coords
        mat[coord] = 1
    end
    return mat
end

function fold_x(m, x)
    axis = x + 1 # 1 indexing
    left = m[:,begin:axis-1]
    right = m[:,axis+1:end]
    size_diff = size(left, 2) - size(right, 2)
    # println(size_diff)
    # println("left: $left, right: $right")
    # if size_diff > 0, left is longer and right needs lengthening
    if size_diff > 0
        println("left is longer")
        right = [right zeros(Int, size(right, 1), size_diff)]
    # else, right is longer and left needs lengthening
    elseif size_diff < 0
        println("right is longer")
        left = [zeros(Int, size(left, 1), abs(size_diff)) left]
    end
    return left + reverse(right, dims = 2)
end

function fold_y(m, y)
    # fold it up
    axis = y + 1 # 1 indexing
    up = m[begin:axis-1,:]
    down = m[axis+1:end,:]
    size_diff = size(up, 1) - size(down, 1)
    # if size_diff > 0, up is longer and down needs lengthening
    if size_diff > 0
        down = [down; zeros(Int, size_diff, size(down, 2))]
    # else, down is longer and up needs lengthening
    elseif size_diff < 0
        up = [up; zeros(Int, abs(size_diff), size(up, 2))]
    end
    return up + reverse(down, dims = 1)
end

function solve_a(io::String)
    coords, folds, m_size = ingest(io)
    mat = make_matrix(coords, m_size) |> permutedims
    if folds[1][1] == "y"
        folded = fold_y(mat, folds[1][2])
    else
        folded = fold_x(mat, folds[1][2])
    end
    return sum(folded .> 0)
end

function solve_b(io::String)
    coords, folds, s = ingest(io)
    mat = make_matrix(coords, s) |> permutedims
    for (index, fold) in enumerate(folds)
        if fold[1] == "y"
            mat = fold_y(mat, fold[2])
        else
            mat = fold_x(mat, fold[2])
        end
    end
    return mat .> 0
end
