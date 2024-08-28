using Core.Compiler: LineInfoNode
import Base: push!, insert!, getindex, setindex!, iterate, length


struct 
    Undefined
end
const undef = Undefined()


struct Variable
    id:: Int
end
var(id::Integer) = Variable(id) # function


struct Branch
    condition::Any
    block::Int
    args::Vector{Any}
end
Branch(br::Branch; condition = br.condition, block = br.block, args = br.args) = Branch(condition, block, args)
# isreturn(b::Branch) = b.block == 0 && length(b.args) == 1
# returnvalue(b::Branch) = b.args[1]
isconditional(b::Branch) = b.condition != nothing
Base.:(==)(a::Branch, b::Branch) = (a.condition == b.condition) && (a.block == b.block) && (a.args == b.args)
Base.copy(br::Branch) = Branch(br.condition, br.block, copy(br.args))
arguments(b::Branch) = b.args
const unreachable = Branch(nothing, 0, [])


struct Statement
    expr::Any
    type::Any
    line::Int
end
Statement(x; expr = x, type = Any, line = 0) = Statement(expr, type, line)
Statement(x::Statement; expr = x.expr, type = x.type, line = x.line) = Statement(expr, type, line)
const stmt = Statement


struct BasicBlock
    stmts::Vector{Statement}
    args::Vector{Any}
    argtypes::Vector{Any}
    branches::Vector{Branch}
end
BasicBlock(stmts = []) = BasicBlock(stmts, [], [], Branch[])
Base.copy(bb::BasicBlock) = BasicBlock(copy(bb.stmts), copy(bb.args), copy(bb.argtyps), copy(bb.branches))
branches(bb::BasicBlock) = bb.branches
arguments(bb::BasicBlock) = bb.args
argtypes(bb::BasicBlock) = bb.argtypes

struct IR
    defs::Vector{Tuple{Int, Int}}
    blocks::Vector{BasicBlock}
    lines::Vector{LineInfoNode}
    meta::Any
end
# IR(; meta = nothing) = IR([], [BasicBlock()], [], meta)
IR(lines::Vector{LineInfoNode}; meta = nothing) = IR([], [BasicBlock()], lines, meta)
Base.copy(ir::IR) = IR(copy(ir.defs), copy(ir.blocks), copy(ir.lines), copy(ir.meta))
length(ir::IR) = sum(x -> x[2] > 0, ir.defs, init = 0)


function block!(ir::IR, i = length(blocks(ir)) + 1)
    insert!(ir.blocks, i, BasicBlock())
    if i != length(blocks(ir))
        for b in blocks(ir), bi = 1:length(branches(b))
            br = branches(b)[bi]
            br.block >= i && (branches(b)[bi] = Branch(br, block = br.block + 1))
        end

        for (ii, (b, j)) = enumerate(ir.defs)
            b >= i && (ir.defs[ii] = (b + 1, j))
        end
    end
    return block(ir, i)
end

function deleteblock!(ir::IR, i::Integer)
    deleteat!(ir.blocks, i)
    if i != length(blocks(ir) + 1)
        for b in blocks(ir), bi = 1:length(branches(b))
            br = branches(b)[bi]
            br.block >= i && (branches(b)[bi] = Branch(br, block = br.block - 1))
        end
    end

    for (ii, (b, j)) = enumerate(ir.defs)
        b == i && (ir.defs[ii] = ( -1, -1))
        b > i && (ir.defs[ii] = (b - 1, j))
    end
    return
end

struct Block
    ir::IR
    id::Int
end
BasicBlock(b::Block) = b.ir.blocks[b.id]
branches(b::Block) = branches(BasicBlock(b))
branches(ir::IR) = length(blocks(ir)) == 1 ? branches(block(ir, 1)) : error("IR has multiple blocks, so `branches(ir)` is ambiguous.")
arguments(b::Block) = arguments(BasicBlock(b))
arguments(ir::IR) = arguments(block(ir, 1))
argtypes(b::Block) = argtypes(BasicBlock(b))
argtypes(ir::IR) = argtypes(block(ir, 1))
canbranch(b::Block) = length(branches(b)) == 0 || isconditional(branches(b)[end])
isreturn(b::Block) = any(isreturn, branches(b))


function explicitbranch!(b::Block)
    b.id == 1 && return
    a = block(b.ir, b.id-1)
    if all(isconditional, branches(a))
        branch!(a, b.id)
    end
    return b
end
  
explicitbranch!(ir::IR) = (foreach(explicitbranch!, blocks(ir)); return ir)

function branches(b::Block, c::Block)
    c.id == b.id+1 && explicitbranch!(c)
    filter(br -> br.block == c.id, branches(b))
end
  
branches(b::Block, c::Integer) = branches(b, block(b.ir, c))
  
function returnvalue(b::Block)
    isreturn(branches(b)[end]) || error("Block does not return")
return returnvalue(branches(b)[end])
end

returntype(b::Block) = exprtype(b.ir, returnvalue(b))


function argument!(b::Block, value = nothing, t = Any;
                   insert = true, at = length(arguments(b))+1, type = t)
    if at < length(arguments(b))
        for i = 1:length(b.ir.defs)
            (c, j) = b.ir.defs[i]
            c == b.id && -j >= at && (b.ir.defs[i] = (c, j-1))
        end
    end
    push!(b.ir.defs, (b.id, -at))
    arg = var(length(b.ir.defs))
    insert!(arguments(b), at, arg)
    insert!(BasicBlock(b).argtypes, at, type)
    if insert
        explicitbranch!(b)
        for c in blocks(b.ir), br in branches(c)
            br.block == b.id && insert!(arguments(br), at, value)
        end
    end
  return arg
end

argument!(ir::IR, a...; kw...) = argument!(block(ir, 1), nothing, a...; kw..., insert = false)

function emptyargs!(b::Block)
    empty!(arguments(b))
    for c in blocks(b.ir), br in branches(c)
        br.block == b.id && empty!(arguments(br))
    end
    return
end

function deletearg!(b::Block, i::Integer)
    arg = arguments(b)[i]
    deleteat!(arguments(b), i)
    deleteat!(argtypes(b), i)
    for c in blocks(b.ir), br in branches(c)
        br.block == b.id && deleteat!(arguments(br), i)
    end
    b.ir.defs[arg.id] = (-1, -1)
    for arg in arguments(b)[i:end]
        (bl, pos) = b.ir.defs[arg.id]
        b.ir.defs[arg.id] = (bl, pos+1)
    end
    return
end

function deletearg!(b::Block, i::AbstractVector)
    for i in sort(i, lt = >)
        deletearg!(b, i)
    end
end


deletearg!(ir::IR, i) = deletearg!(block(ir, 1), i)
block(ir::IR, i) = Block(ir, i)
block(ir::IR, v::Variable) = blockidx(ir, v)[1]
blocks(ir::IR) = [block(ir, i) for i = 1:length(ir.blocks)]

function blockidx(ir::IR, x::Variable)
    b, i = get(ir.defs, x.id, (-1, -1))
    i > 0 || error("No such variable $x")
    block(ir, b), i
end


function Base.haskey(ir::IR, x::Variable)
    b, i = get(ir.defs, x.id, (-1, -1))
    return i > 0
end

getindex(b::Block, i::Integer) = BasicBlock(b).stmts[i]
getindex(b::Block, i::Variable) = b.ir[i]
setindex!(b::Block, x::Statement, i::Integer) = (BasicBlock(b).stmts[i] = x)
setindex!(b::Block, x, i::Integer) = (b[i] = Statement(b[i], expr = x))
branch(block::Integer, args...; unless = nothing) = Branch(unless, block, Any[args...])
branch(block::Block, args...; kw...) = branch(block.id, args...; kw...)


function branch!(b::Block, block, args...; unless = nothing)
    brs = branches(b)
    unless === nothing && deleteat!(brs, findall(br -> br.condition === nothing, brs))
    args = map(a -> a isa Expr ? push!(b, a) : a, args)
    push!(brs, branch(block, args...; unless = unless))
    return b
end


function branch!(ir::IR, args...; kw...)
    branch!(blocks(ir)[end], args...; kw...)
    return ir
end

return!(ir, x) = branch!(ir, 0, x)

function getindex(ir::IR, i::Variable)
    b, i = blockidx(ir, i)
    return b[i]
end

Base.get(ir::IR, i::Variable, default) = haskey(ir, i) ? ir[i] : default

function setindex!(ir::IR, x, i::Variable)
      b, i = blockidx(ir, i)
    b[i] = x
end

setindex!(b::Block, x, i::Variable) = setindex!(b.ir, x, i)

function Base.delete!(ir::IR, i::Variable)
    ir[i] = nothing
    ir.defs[i.id] = (-1, -1)
    return ir
end

Base.delete!(b::Block, i::Variable) = delete!(b.ir, i)
length(b::Block) = count(x -> x[1] == b.id, b.ir.defs)

function successors(b::Block)
    brs = BasicBlock(b).branches
    succs = Int[br.block for br in brs if br.block > 0]
    all(br -> br.condition != nothing, brs) && b.id < length(blocks(b.ir)) && push!(succs, b.id+1)
    return [block(b.ir, succ) for succ in succs]
end


predecessors(b::Block) = [c for c in blocks(b.ir) if b in successors(c)]
Base.keys(b::Block) = first.(sort([Variable(i) => v for (i, v) in enumerate(b.ir.defs) if v[1] == b.id && v[2] > 0], by = x->x[2]))

function iterate(b::Block, (ks, i) = (keys(b), 1))
    i > length(ks) && return
    return (ks[i]=>b.ir[ks[i]], (ks, i+1))
end

Base.keys(ir::IR) = first.(sort([Variable(i) => v for (i, v) in enumerate(ir.defs) if v[2] > 0], by = x->x[2]))

function iterate(ir::IR, (ks, i) = (keys(ir), 1))
    i > length(ks) && return
    return (ks[i]=>ir[ks[i]], (ks, i+1))
end

applyex(f, x) = x
applyex(f, x::Expr) = Expr(x.head, [x isa Expr ? f(x) : x for x in x.args]...)
applyex(f, x::Statement) = Statement(x, expr = applyex(f, x.expr))

function push!(b::Block, x::Statement)
    if !isexpr(x.expr, :foreigncall) # needed to avoid https://github.com/MikeInnes/IRTools.jl/issues/30
        x = applyex(a -> push!(b, Statement(x, expr = a)), x)
    end
    x = Statement(x)
    push!(BasicBlock(b).stmts, x)
    push!(b.ir.defs, (b.id, length(BasicBlock(b).stmts)))
    return Variable(length(b.ir.defs))
end

push!(b::Block, x) = push!(b, Statement(x))
push!(b::Block, x::Variable) = x

# TODO make this work on nested Exprs.
function insert!(b::Block, idx::Integer, x)
    insert!(BasicBlock(b).stmts, idx, Statement(x))
    for i = 1:length(b.ir.defs)
        c, j = b.ir.defs[i]
        if c == b.id && j >= idx
            b.ir.defs[i] = (c, j+1)
        end
    end
    push!(b.ir.defs, (b.id, idx))
    return Variable(length(b.ir.defs))
end

Base.pushfirst!(b::Block, x) = insert!(b, 1, x)
push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)
Base.pushfirst!(ir::IR, x) = pushfirst!(block(ir, 1), x)


function insert!(ir::IR, i::Variable, x; after = false)
    if after && ir.defs[i.id][2] < 0
        pushfirst!(block(ir, ir.defs[i.id][1]), x)
    else
    b, i = blockidx(ir, i)
    insert!(b, i+after, x)
    end
end

insert!(b::Block, i::Variable, x; after = false) = insert!(b.ir, i, x; after = after)
insertafter!(ir, i, x) = insert!(ir, i, x, after=true)

struct NewVariable
    id::Int
end

# PIPELINE
mutable struct Pipe
    from::IR
    to::IR
    map::Dict{Any,Any}
    var::Int
    branch
    block
end

var!(p::Pipe) = NewVariable(p.var += 1)
substitute!(p::Pipe, x, y) = (p.map[x] = y; x)
substitute(p::Pipe, x::Union{Variable,NewVariable}) = p.map[x]
substitute(p::Pipe, x) = get(p.map, x, x)
substitute(p::Pipe, x::Statement) = stmt(x, expr = substitute(p, x.expr))
substitute(p::Pipe, x::Expr) = Expr(x.head, substitute.((p,), x.args)...)
substitute(p::Pipe) = x -> substitute(p, x)

function Pipe(ir)
    p = Pipe(ir, IR(copy(ir.lines), meta = ir.meta), Dict(), 0, identity, identity)
    for (x, T) in zip(p.from.blocks[1].args, p.from.blocks[1].argtypes)
        y = argument!(blocks(p.to)[end], nothing, T, insert = false)
        substitute!(p, x, y)
    end
    return p
end

function pipestate(ir::IR)
    ks = sort([Variable(i) => v for (i, v) in enumerate(ir.defs) if v[2] > 0], by = x->x[2])
    [first.(filter(x -> x[2][1] == b, ks)) for b = 1:length(ir.blocks)]
end

branches(f, p::Pipe) = (p.branch = f)
blocks(f, p::Pipe) = (p.block = f)

function iterate(p::Pipe, (ks, b, i) = (pipestate(p.from), 1, 1))
    i == 1 && b == 1 && p.block(b)
    if i == 1 && b != 1
        for (x, T) in zip(p.from.blocks[b].args, p.from.blocks[b].argtypes)
            y = argument!(blocks(p.to)[end], nothing, T, insert = false)
            substitute!(p, x, y)
        end
    end
    if i > length(ks[b])
        for br in branches(block(p.from, b))
            br′ = p.branch(br)
            br′ == nothing || push!(p.to.blocks[end].branches, map(substitute(p), br′))
        end
        b == length(ks) && return
        p.block(b)
        block!(p.to)
        return iterate(p, (ks, b+1, 1))
    end
    v = ks[b][i]
    st = p.from[v]
    substitute!(p, v, push!(p.to, substitute(p, st)))
    ((v, st), (ks, b, i+1))
end

finish(p::Pipe) = p.to
islastdef(ir::IR, v::Variable) =v.id == length(ir.defs) && ir.defs[v.id] == (length(ir.blocks), length(ir.blocks[end].stmts))
setindex!(p::Pipe, x, v) = p.to[substitute(p, v)] = substitute(p, x)

function setindex!(p::Pipe, x::Variable, v)
    v′ = substitute(p, v)
    if islastdef(p.to, v′)
        delete!(p, v)
        substitute!(p, v, substitute(p, x))
    else
        p.to[v′] = substitute(p, x)
    end
end

function Base.push!(p::Pipe, x)
    tmp = var!(p)
    substitute!(p, tmp, push!(p.to, substitute(p, x)))
    return tmp
end

function Base.pushfirst!(p::Pipe, x)
    tmp = var!(p)
    substitute!(p, tmp, pushfirst!(p.to, substitute(p, x)))
    return tmp
end

function Base.delete!(p::Pipe, v)
    v′ = substitute(p, v)
    delete!(p.map, v)
    if islastdef(p.to, v′)
        pop!(p.to.defs)
        pop!(p.to.blocks[end].stmts)
    else
        delete!(p.to, v′)
    end
end


