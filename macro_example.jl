function show(x)
    println("The value you passed is ", x)

end
orange = "sweet"
show(orange)

macro show_value(x)
    println(typeof(x))
    #println($(esc(x))) # this will be an error since $ need to be in quote
    println(:($(esc(x))))
    y = :($esc(x))
    # print(eval(y))
    quote
        # println("The ", $string(x), " you passed is ", $esc(x))
        println("The ", $(string(x)), " you passed is ", $(esc(x)))
    end
    # return :( println("The value you passed is ", $x) )
end

@show_value(orange)


println("======================")
module SomeModule
    # orange = "sweet"
    export @show_value_no_esc
    macro show_value_no_esc(variable)
        quote
            println("The ", $(string(variable)), " you passed is ", $variable)
        end
    end
end

using .SomeModule

try
    @show_value_no_esc(orange)
catch e
    println(sprint(showerror, e))
end
println(@macroexpand @show_value_no_esc(orange))



macro fill(exp, sizes...)
    println(map(sizes) do s end)
    println(map(sizes))
    iterator_expressions = map(sizes) do s
        Expr(
            :(=),
            :_,
            quote 1:$(esc(s)) end
        )
    end
    
    Expr(
        :comprehension,
        esc(exp),
        iterator_expressions...
    )
end

println(@fill(rand(3), 5))