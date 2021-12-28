# 1. parse input
# how do I turn the matrix into edges?
# how do I create a data structure for a weighted graph?
# 2. create a data graph structure

function parser(io::IO)
    text = readlines(io)
    matrix = zeros(Integer, length(text), length(first(text)))
    reduce(hcat, [parse.(Integer, collect(line)) for line = text])
end
