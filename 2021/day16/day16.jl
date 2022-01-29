binToDec(x::Int) = parse(Int, string(x), base=2)

test = "D2FE28"

hexDict = Dict(
    '0' => "0000",
    '1' => "0001",
    '2' => "0010",
    '3' => "0011",
    '4' => "0100",
    '5' => "0101",
    '6' => "0110",
    '7' => "0111",
    '8' => "1000",
    '9' => "1001",
    'A' => "1010",
    'B' => "1011",
    'C' => "1100",
    'D' => "1101",
    'E' => "1110",
    'F' => "1111"
)

packet(input::String) = reduce(*, map(x -> hexDict[x], collect(input)))

function decode(packet::String)
    io = IOBuffer(packet)
    version = parse(Int, String(read(io, 3)), base = 2)
    typeid = parse(Int, String(read(io, 3)), base = 2)
    bits = ""
    if typeid == 4
        while true
            if String(read(io, 1)) == "1"
                bits *= String(read(io, 4))
            else
                bits *= String(read(io, 4))
                break
            end
        end
    end
    return (version, typeid, parse(Int, bits, base=2))
end
