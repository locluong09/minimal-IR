g(arg1, arg2, arg3...) = arg1(arg2, arg3)
g1(arg1, arg2, arg3, arg4...) = arg1, arg2, arg3
g2(arg1, arg2, arg3...) = arg1, arg2, arg3
g3(x,y,z...) = x+ y+ sum(z)


a,b,c= g3(10, 2, 3, 5) do x,y #<-anon.fn as 
    x, y, map(z -> x*z, y) # first arg.
end[1:3] # <- discard third value returned #> (17,(329,932))
println(a)
println(b)
println(c)

println("======================")
k(x,y...) = x + sum(y)
t(x,y...) = sum(map(z -> z^2, y))
h(arg1, arg2, arg3) = arg1 + arg2
println(k(1,2,3,4))
println(t(1,2,3,4))
println(h(1,2, 3))

A = [1,2,3]

map(A) do x
    x ^ 2
end

map(x -> x ^ 2, A)

