input = readlines("input")
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
boards = reshape(boards_concat, (5, 5, div(n,m)))
results = NamedTuple{(:call, :value, :board, :sum, :score), NTuple{5, Int}}[]
for b in 1:size(boards, 3)
    matches = zeros(Bool, size(b))
    for (call, value) in enumerate(calls)
        matches = matches .| (boards[:,:,b] .== value)
        colsum = sum(matches, dims = 1) .== 5
        rowsum = sum(matches, dims = 2) .== 5
            if any(colsum) | any(rowsum)
                unmarked = boards[.!matches, b]
                s = sum(unmarked)
                push!(results, (call = call, value = value, board = b, sum = s, score = s * value))
                break
            end
    end
end
first_to_win = sort(results, by = x -> x[:call])[1]
last_to_win = sort(results, by = x -> x[:call], rev = true)[1]
