using BenchmarkTools

function parse_binary(x::BitMatrix)
    # convert from these booleans to a vector of Int
    v = convert.(Int, x)
    return parse(Int, join(v), base = 2)
end

function parse_binary(x::Matrix)
    # convert from these booleans to a vector of Int
    return parse(Int, join(x), base = 2)
end

function get_power(x::Matrix)
    m, n = size(x)
    γ = sum(x, dims = 1) .> m/2
    ϵ = .!γ
    return parse_binary.((γ, ϵ))
end

function get_rating(x::Matrix)
    # for each array, find out which one is most common
   function filter_rating(x::Matrix, y::String)
        m, n = size(x)
        for v in 1:n
            x_array = x[:,v]
            m = length(x_array)
            if y == "oxy"
                x_maj = sum(x_array) >= m/2
            else
                x_maj = sum(x_array) < m/2
            end
            if size(x)[1] == 1 break end
            x = x[x_array .== x_maj,:]
        end
        return x
    end
    oxy = filter_rating(x, "oxy")
    co2 = filter_rating(x, "co2")
    return parse_binary.((oxy, co2))
end

function main(path)
    input = readlines(path)
    # parse each line to an array
    parsed = map(f -> parse.(Int, f), collect.(input))
    # concat vectors and transpose (but use permutedims for it)
    bits = permutedims(reduce(hcat, parsed))
    γ, ϵ = get_power(bits)
    oxy, co2 = get_rating(bits)
    power_consumption = γ * ϵ
    life_support = oxy * co2
    return(power_consumption, life_support)
end

println(main("input"))
@benchmark main("input")
