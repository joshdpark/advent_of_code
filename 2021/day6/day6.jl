function growth_model(state::Vector{Int}, days::Int)
    for x in 1:days
        if 0 in state
            zero_ind = findall(x -> x == 0, state)
            append!(state, fill(9, length(zero_ind)))
            state[zero_ind] .= 7
        end
        state .-= 1
    end
    return state
end
function main(path, days)
    state = parse.(Int, split(readlines(path)[1],","))
    growth_model(state, days)
end
@time length(main("test", 80))
@assert length(main("test", 18)) == 26
@assert length(main("test", 80)) == 5934

# issue: running out of memory: need to figure out a memory efficient way to check this
function main(input, days)
    fish = parse.(Int, split(readlines(input)[1],","))
    function count_fish(fish, days)
        counter = [count(==(n), fish) for n in 0:8]
        for _ in 1:days
            n = counter[1]
            counter = circshift(counter, -1)
            counter[7] += n
            counter
        end
        sum(counter)
    end
    return count_fish(fish, days)
end
main("test", 80)
@assert main("test", 256) == 26984457539
main("test", 256)
main("input", 80)
main("input", 256)
