using Core: CodeInfo, Typeof
using Core.Compiler: InferenceState, MethodInstance, svec
using InteractiveUtils: typesof

if isdefined(Base, :hasgenerator)
    hasgenerator(x) = Base.hasgenerator(x)
else
    hasgenerator(x) = Base.isgenerated(x)
end

worldcounter() = ccall(:jl_get_world_counter, UInt, ())


struct Meta
    method::Method
    instance::MethodInstance
    code::CodeInfo
    nargs::Int
    sparams
end

untvar(t::TypeVar) = t.ub
untvar(x) = x
const spoofed_world = Ref{Union{Nothing, UInt}}(nothing)


function meta(T; types = T, world = nothing)
    if world == nothing
        world = something(spoofed_world[], worldcounter())
    end

    F = T.parameters[1]
    # F == typeof(invoke) && return invoke_meta(T; world = world)
    F isa DataType && (F.name.module === Core.Compiler || F<: Core.Builtin || F<: Core.Builtin) && return nothing

    min_world = Ref{UInt}(typemin(UInt))
    max_world = Ref{UInt}(typemax(UInt))
    has_ambig = Ptr{Int32}(C_NULL)
    _methods = if VERSION >= v"1.7.0-DEV.1297"
        Base._methods_by_ftype(T, nothing, -1, world, false, min_world, max_world, has_ambig)
    else
        Base._methods_by_ftype(T, -1, world, false, min_world, max_world)
    end

    _methods === nothing && return nothing
    _methods isa Bool && return nothing
    length(_methods) == 0 && return nothing
    type_signature, sps, method = last(_methods)
    sps = svec(map(untvar, sps)...)
    @static if VERSION >= v"v1.2-"
        mi = Core.Compiler.specialize_method(method, types, sps)
        ci = hasgenerator(mi) ? get_stagged(mi, world) : Base.uncompressed_ast(method)
    else
        mi = Core.Compiler.code_for_method(method, types, sps, world, false)
        ci = hasgenerator(mi) ? get_stagged(mi, world) : Base.uncompressed_ast(mi)
    end
    Base.Meta.partially_inline!(ci.code, [], method.sig, Any[sps...], 0, 0, :propagate)
    Meta(method, mi, ci, method.nargs, sps)
end


# macro meta(ex...)
#     if length(ex) == 1
#         world = nothing
#         call = ex[1]
#     elseif length(ex) == 2
#         world, call = ex
#     else
#         error("@meta [world] f(args...)")
#     end
#     isexpr(call, :call) || error("@meta [world] f(args...)")
#     f, args = call.args[1], call.args[2:end]
#     :(meta(typesof($(esc.((f, args...))...)); world=$(esc(world))))
# end


function code_ir(f, T)
    m = meta(Tuple{Typeof(f), T.parameters...})
    return IR(m)
end

function code_irm(ex)
    if isexpr(ex, :call)
        f, args = ex.args[1], ex.args[2:end]
    elseif isexpr(ex, :do)
        f, args = ex.args[1].args[1], vcat(ex.args[2], ex.args[1].args[2:end])
    else
        error("Invalid expression")
    end
    :($code_ir($(esc(f)), typesof($(esc.(args)...))))
end

macro code_ir(ex)
    code_irm(ex)
end

println(1)

  