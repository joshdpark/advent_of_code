function heapifyfirst!(q)
    L = length(q)
    n = 1
    while true
        n′ = n
        l = 2 * n
        r = l + 1
        if l ≤ L && first(q[n′]) > first(q[l])
            n′ = l
        end
        if r ≤ L && first(q[n′]) > first(q[r])
            n′ = r
        end
        if n != n′
            q[n], q[n′] = q[n′], q[n]
            n = n′
        else
            break
        end
    end
end

function heapifylast!(q)
    n = length(q)
    while n > 1 && first(q[n ÷ 2]) > first(q[n])
        q[n ÷ 2], q[n] = q[n], q[n ÷ 2]
        n ÷= 2
    end
end

function solve1(input)
    W, H = size(input)
    seen = falses(W, H)
    risk = zeros(Int, W, H)
    q = [0 => (1, 1)]
    while !seen[W, H]
        r, (i, j) = q[1]
        q[1] = q[end]
        pop!(q)
        heapifyfirst!(q)
        for (δᵢ, δⱼ) in ((-1, 0), (1, 0), (0, -1), (0, 1))
            i′ = i + δᵢ
            j′ = j + δⱼ
            if 1 ≤ i′ ≤ W && 1 ≤ j′ ≤ H && !seen[i′, j′]
                seen[i′, j′] = true
                r′ = r + input[i′, j′]
                risk[i′, j′] = r′
                push!(q, r′ => (i′, j′))
                heapifylast!(q)
            end
        end
    end
    risk[W, H]
end

f(x, y) = ((x + y) % 9) + 1

function solve2(input)
    input′ = [input f.(input, 0) f.(input, 1) f.(input, 2) f.(input, 3)
              f.(input, 0) f.(input, 1) f.(input, 2) f.(input, 3) f.(input, 4)
              f.(input, 1) f.(input, 2) f.(input, 3) f.(input, 4) f.(input, 5)
              f.(input, 2) f.(input, 3) f.(input, 4) f.(input, 5) f.(input, 6)
              f.(input, 3) f.(input, 4) f.(input, 5) f.(input, 6) f.(input, 7)]
    solve1(input′)
end

function main(file)
    input = hcat([[parse(Int, ch) for ch in line] for line in eachline(file)]...)
    println(solve1(input))
    println(solve2(input))
end

function parta(file)
    input = hcat([[parse(Int, ch) for ch in line] for line in eachline(file)]...)
    solve1(input)
end

function partb(file)
    input = hcat([[parse(Int, ch) for ch in line] for line in eachline(file)]...)
    solve2(input)
end
