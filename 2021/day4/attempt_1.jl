function main()
    input = readlines("test2")
    calls = parse.(Int, split(input[1], ","))
    # get boards
    board = Vector{Int}[]
    for l in input[2:end]
        if l != ""
            push!(board, parse.(Int, split(l)))
        end
    end
    # format the matrix correctly; not sure how this ended up working
    boards_concat = reduce(hcat, board)
    m, n = size(boards_concat)
    boards = permutedims(reshape(boards_concat, (5, 5, Int(n/m))), (2, 1, 3))
    matches = zeros(Bool, size(boards))
    for (index, value) in enumerate(calls)
        matches = matches .| (boards .== value)
        for i in 1:ndims(boards)
            colsum = sum(matches[:,:,i], dims = 1)
            rowsum = sum(matches[:,:,i], dims = 2)
            if (5 in rowsum) | (5 in colsum)
                unmarked = boards[.!matches[:,:,i], i]
                s = sum(unmarked)
                println("call: $index, value : $value, board: $i, sum: $s, unmarked: $unmarked")
                print(boards[:,:,i], "\n")
                return s * value
            end
        end
    end
end
main()
