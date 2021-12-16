raw = readlines("input.txt")

# parse to integers from string
depths = parse.(Int, raw)

function depth_increase(x)
    counter = 0
    for (i, v) in enumerate(x)
        if i == 1
            continue
        end
        if v > x[i-1]
            counter = counter + 1
        end
    end
    return counter
end

part_a = depth_increase(depths)
println("The number of increases for part a is $part_a")

function depth_increase_three(x)
    window = Int[]
    for (i, v) in enumerate(x)
        # break if the index + 2 would exceed vector length
        if i+2 > length(x)
            break
        end
        sum = v + x[i+1] + x[i+2]
        push!(window, sum)
    end
    counter = depth_increase(window)
    return counter
end

part_b = depth_increase_three(depths)
println("The number of increases for part a is $part_b")
