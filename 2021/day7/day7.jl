# Approach: use a distance matrix and get the results of distance for each position
# example
# A = [0 1 2
#      1 0 1
#      2 1 0]
#The input = [1, 3, 3]
# s = [1, 0, 2]
# the array of distances = A * s
# get the min

using BenchmarkTools
input = readlines("input")
# parse input to array
initial = parse.(Int, split(input[1], ","))
# create a 1xlength(input) matrix
mat = zeros(Int, maximum(initial) + 1)
for (index,value) = enumerate(initial)
    mat[value + 1] += 1
end
# create a distance matrix
max_dist = maximum(initial)
A = reduce(hcat, [abs.((collect(0:max_dist) .- x)) for x = 0:max_dist])
s = A * mat
minimum(s)

# create a sequence of the ever enlarging + 1 cost for each + 1 in position
gauss(n) = Int(n*(n + 1)/2)
A_2 = gauss.(A)
s_2 = A_2 * mat
minimum(s_2)


