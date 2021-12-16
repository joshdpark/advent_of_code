using LinearAlgebra

# read in the data
# data = readlines("input")
function asteroids(input)
    data = readlines(input)

    arrays = []
    # convert to vectors
    for (j, line) in enumerate(data)
        for (i, l) in enumerate(line)
            if l == '#'
                # 1 indexing
                push!(arrays, [i - 1, j - 1])
            end
        end
    end

    function magnitude(p)
        sqrt(sum(p.^2.0))
    end

    function ang(p1, p2)
        acosd(dot(p1, p2)/(magnitude(p1) * magnitude(p2)))
    end

    # make a subset
    subset = arrays
    # println(subset)
    list = []
    # for each point, offset origin and get all unique slope values as vectors
    for (i, origin) in enumerate(subset)
        offset = [a - origin for a in subset]
        # get the angle between the [1,0] and the offset vector
        slope = [ a / gcd(a) for a in offset where a != [0, 0]]
        asteroids = length(unique(slope))
        # println(asteroids)
        push!(list, [origin, asteroids])
    end
    # get the maximum number of asteroids seen
    m = maximum([l[2] for l in list])
    println("There are $m asteroids")
    location = filter(f -> f[2] == m, list)[1][1]
    println("The location is: $location")
    return m
end

# the example answer
asteroids("sample1") == 8
asteroids("sample2") == 33
asteroids("sample3") == 35
asteroids("sample4") == 41
asteroids("sample5") == 210

asteroids("input")
