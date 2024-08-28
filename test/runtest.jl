using MacroTools
using Test

include("../src/IRTools.jl")
using .IRTools

using .IRTools: IR

@testset "IR" begin
  include("ir.jl")
end
