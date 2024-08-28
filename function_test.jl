function f(arg1, arg2)
    if arg1 isa Int64
        return arg1
    elseif arg2 isa Function 
        return arg2(arg1)
    else
        arg1, arg2
    end
end

f(x) = x + 2
println(f(2.0, x -> x^2))
println(f(1.0, f))

g(arg1, arg2, arg3...) = arg1(arg2, arg3)
a,b,c = g(17, 329, 932) do x,y #<-anon.fn as 
    x, y, map(z -> x*z, y) # first arg.
end[1:3] # <- discard third value returned #> (17,(329,932))
println(a)
println(b)
println(c)

h(A,B,C) = map(x->begin
        if x < 0 && iseven(x)
            return 0
        elseif x == 0
            return 1
        else
            return x
        end
    end,
    [A, B, C])

println(h(-2,0,3))

map([1, 2, 3]) do x
    if x < 0 && iseven(x)
        return 0
    elseif x == 0
        return 1
    else
        return x
    end
end

println(map(x -> x^2, [1,2,3]))
