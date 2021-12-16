function a(position, depth)
    x = direction == "forward" ? distance : 0
    y = direction == "up" ? distance : 0
    y = direction == "down" ? distance : 0
    return x, y
end

begin
    input = Tuple{String, Int}[]
    for line in eachline("input")
        local s = split(line, ' ')
        push!(input, (s[1], parse(Int, s[2])))
    end
    # part a
    part_a = let
        local x_position = 0
        local y_position = 0
        for (direction, distance) in input
            if direction == "forward"
                x_position += distance
            elseif direction == "up"
                y_position -= distance
            elseif direction == "down"
                y_position += distance
            end
        end
        println("x = $x_position, y = $y_position")
        println("part a: $(x_position * y_position)")
    end
    # part b
    part_b = let
        local aim = 0
        local x_position = 0
        local y_position = 0
        # part b
        for (direction, distance) in input
            if direction == "forward"
                x_position += distance
                y_position += distance * aim
            elseif direction == "up"
                aim -= distance
            elseif direction == "down"
                aim += distance
            end
        end
        println("x = $x_position, y = $y_position")
        println("part b: $(x_position * y_position)")
    end
end

