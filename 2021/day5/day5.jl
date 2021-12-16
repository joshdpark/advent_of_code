# read and parse input
function main(path)
    input = readlines(path)
    # get dimensions from each line
    function parse_input(x::String)
        r = replace(x, r" -> " => ",")
        s = split(r, ",")
        i = parse.(Int, s)
        return i .+ 1 # 1 indexing
    end
    mat = parse_input.(input)
    # get the max of the inputs for the matrix
    dim = maximum(unique(vcat(mat...)))
    # for each m in mat fill in vents add 1 to vents
    function fill_vents(v::Vector)
        # subset vents matrix
        # x1,y1 -> x2,y2
        x1,y1,x2,y2 = v[1],v[2],v[3],v[4]
        local I_min, I_max = extrema(v)
        # locate diag
        if abs(x2 - x1) == abs(y2 - y1)
            if ((x2 - x1) * (y2 - y1)) > 0
                I = [CartesianIndex(min(x1,x2) + x, min(y1,y2) + x) for x = 0:abs(x2-x1)]
                vents[I] .+= 1
            else
                I = [CartesianIndex(min(x1,x2) + x, max(y1,y2) - x) for x = 0:abs(x2-x1)]
                vents[I] .+= 1
            end
        elseif (x1 == x2) | (y1 == y2)
            I = CartesianIndex(x1, y1)
            J = CartesianIndex(x2, y2)
            if 0 in size(vents[J:I])
                vents[I:J] .+= 1
            else
                vents[J:I] .+= 1
            end
        end
    end
    # create a matrix for the vents
    vents = zeros(Int, dim, dim)
    fill_vents.(mat)
    return vents, sum(vents .>= 2)
end

# @assert main("test") == 5
# @assert main("input") != 51
vents, answer = main("test")
@assert answer == 12
main("input")

# attempts
# 51: wrong
#
# part 2: write a function that gets the cartesian index based off the x,y that handles daignols
# difficulty: figuring out how to detect diagonals correctly
for i in mat
    x1,y1,x2,y2 = i[1],i[2],i[3],i[4]
    if abs(x2 - x1) == abs(y2 - y1)
        println(i, "diagonal")
        if ((x2 - x1) * (y2 - y1)) > 0
            I = [CartesianIndex(min(x1,x2) + x, min(y1,y2) + x) for x = 0:abs(x2-x1)]
            println("\\ diagonal", I)
        else
            I = [CartesianIndex(min(x1,x2) + x, max(y1,y2) - x) for x = 0:abs(x2-x1)]
            println("/ diagonal", I)
        end
    end
end

for i in mat
    i = reshape(i, (2, 2))
    println(sum(i, dims = 1))
    println(sum(i, dims = 2))
end
