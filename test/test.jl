using MacroTools
using Test

include("../src/IRTools.jl")
using .IRTools

using .IRTools: IR

function f(x)
    N = 4
    for i1 in 1:3    # single loop works without issue
        for i2 in 1:N   # needs to be a variable `1:4` works fine
        end
    end
    0.0 # same error with `x`
end

println(@code_ir f(1))

function f(a, b)
    u = 1
    while true
        if true
        end
    end
    f(u)
end

# @test @code_ir(f(1, 2)) isa IR
bar(a, b) = a > b ? a : b
function foo(a, b)
    bar(a, b)
  end

# println(@code_ir foo(1, 2))
println(@code_ir(foo(1.0, 2.0)))