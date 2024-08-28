import Base: map, map!
import Core.Compiler: PhiNode, PiNode, ssamap, userefs
import MacroTools: walk, prewalk, postwalk

walk(x::PhiNode, inner, outer) = outer(PhiNode(x.edges, [inner(isassigned(x.values, i) ? x.values[i] : undef) for i in 1:length(x.values)]))
walk(x::PiNode, inner, outer) = outer(PiNode(inner(x.val), x.typ))
xcall(mod::Module, f::Symbol, args...) = Expr(:call, GlobalRef(mod, f), args...)
xcall(f::Symbol, args...) = xcall(Base, f, args...)
xcall(f, args...) = Expr(:call, f, args...)
map(f, br::Branch) = Branch(br, condition = f(br.condition), args = f.(br.args))

function map(f, b::BasicBlock)
  stmts = map(x -> Statement(x, expr = f(x.expr)), b.stmts)
  branches = map(br -> map(f, br), b.branches)
  BasicBlock(stmts, b.args, b.argtypes, branches)
end

function map!(f, b::BasicBlock)
  map!(x -> Statement(x, expr = f(x.expr)), b.stmts, b.stmts)
  map!(br -> Branch(br, condition = f(br.condition), args = f.(br.args)), b.branches, b.branches)
  return b
end

function map!(f, b::Block)
  map!(f, BasicBlock(b))
end

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks), ir.lines, ir.meta)
end

function map!(f, ir::IR)
  for b in blocks(ir)
    map!(f, b)
  end
  return ir
end

walk(st::Statement, inner, outer) = Statement(st, expr = inner(st.expr))
walk(bb::BasicBlock, inner, outer) = map(inner, bb)
walk(bb::Branch, inner, outer) = map(inner, bb)
walk(b::Block, inner, outer) = walk(BasicBlock(b), inner, outer)
walk(ir::IR, inner, outer) = outer(map(inner, ir))

# Avoid recursing into lambdas
prewalk(f, ir::Union{IR,Block})  = walk(f(ir), x -> x isa IR ? x : prewalk(f, x), identity)
postwalk(f, ir::Union{IR,Block}) = walk(ir, x -> x isa IR ? x : postwalk(f, x), f)
prewalk!(f, ir::Union{IR,Block})  = map!(x -> x isa IR ? x :  prewalk(f, x), ir)
postwalk!(f, ir::Union{IR,Block}) = map!(x -> x isa IR ? x : postwalk(f, x), ir)
varmap(f, x) = prewalk(x -> x isa Variable ? f(x) : x, x)

exprtype(x::GlobalRef; typeof = Typeof) = isconst(x.mod, x.name) ? typeof(getfield(x.mod, x.name)) : Any
exprtype(ir::IR, x::GlobalRef; typeof = Typeof) = exprtype(x, typeof = typeof)
exprtype(ir::IR, x::QuoteNode; typeof = Typeof) = typeof(x.value)
exprtype(ir::IR, x::Expr; typeof = Typeof) = error(x)
exprtype(ir::IR, x; typeof = Typeof) = typeof(x)

function exprtype(ir::IR, x::Variable; typeof = Typeof)
  b, i = get(ir.defs, x.id, (-1, -1))
  b == -1 && error("No such variable $x")
  if i > 0
    ir[x].type
  else
    ir.blocks[b].argtypes[-i]
  end
end

function exprline(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  i > 0 || return
  get(ir.lines, ir[x].line, nothing)
end

# COPY from passes to comment out passes.jl

struct CFG
  graph::Vector{Vector{Int}}
end

struct Slot
  id::Symbol
  type
end

Slot(id) = Slot(id, Any)

function usecounts(ir::IR)
  counts = Dict{Variable,Int}()
  prewalk(ir) do x
    x isa Variable && (counts[x] = get(counts, x, 0)+1)
    return x
  end
  return counts
end

function renumber(ir)
  p = Pipe(ir)
  for (v, st) in p
    ex = st.expr
    if isbits(ex) # Trivial expressions can be inlined
      delete!(p, v)
      substitute!(p, v, substitute(p, ex))
    end
  end
  return finish(p)
end

function merge_returns!(ir)
  bs = [b for b in blocks(ir) if isreturn(b)]
  length(bs) == 1 && bs[1] == blocks(ir)[end] && return ir
  block!(ir)
  ret = argument!(blocks(ir)[end])
  return!(ir, ret)
  for b in bs
    branches(b)[end] = branch(length(ir.blocks), arguments(branches(b)[end])[1])
  end
  return ir
end

function expand!(ir::IR)
  worklist = blocks(ir)
  spats = Dict(b => Dict() for b in blocks(ir))
  while !isempty(worklist)
    b = pop!(worklist)
    b.id == 1 && continue
    defs = definitions(b)
    uses = usages(b)
    for v in setdiff(uses, defs)
      haskey(spats[b], v) && continue
      spats[b][v] = argument!(b, v)
      for c in predecessors(b)
        c in worklist || push!(worklist, c)
      end
    end
    ir.blocks[b.id] = prewalk(x -> get(spats[b], x, x), ir.blocks[b.id])
  end
  return ir
end

function prune!(ir::IR)
  usages = usecounts(ir)
  worklist = blocks(ir)
  queue!(b) = (b in worklist || push!(worklist, b))
  function rename!(env, ir)
    for b in blocks(ir)
      prewalk!(b) do x
        haskey(env, x) || return x
        foreach(queue!, successors(b))
        env[x]
      end
    end
  end
  while !isempty(worklist)
    b = popfirst!(worklist)
    isempty(arguments(b)) && continue
    brs = filter(br -> br.block == b.id, [br for a in blocks(ir) for br in branches(a)])
    isempty(brs) && continue
    # Redundant due to all inputs being the same
    inputs = [setdiff(in, (a,)) for (a, in) in zip(arguments(b), zip(arguments.(brs)...))]
    del = findall(x -> length(x) == 1, inputs)
    rename = Dict(zip(arguments(b)[del], first.(inputs[del])))
    if !isempty(rename)
      deletearg!(b, del)
      rename!(rename, ir)
    end
    # Redundant due to not being used
    unused = findall(x -> get(usages, x, 0) == 0, arguments(b))
    if !isempty(unused)
      for a in predecessors(b)
        for br in branches(a, b), i in unused
          arguments(br)[i] isa Variable &&
            (usages[arguments(br)[i]] -= 1)
        end
        a in worklist || push!(worklist, a)
      end
      deletearg!(b, unused)
    end
  end
  return ir
end

function ssa!(ir::IR)
  current = 1
  defs = Dict(b => Dict{Slot,Any}() for b in 1:length(ir.blocks))
  todo = Dict(b => Dict{Int,Vector{Slot}}() for b in 1:length(ir.blocks))
  catches = Dict()
  handlers = []
  function reaching(b, v)
    haskey(defs[b.id], v) && return defs[b.id][v]
    b.id == 1 && return undef
    x = defs[b.id][v] = argument!(b, type = v.type, insert = false)
    for pred in predecessors(b)
      if pred.id < current
        for br in branches(pred, b)
          push!(br.args, reaching(pred, v))
        end
      else
        push!(get!(todo[pred.id], b.id, Slot[]), v)
      end
    end
    return x
  end
  function catchbranch!(v, slot = nothing)
    for handler in handlers
      args = reaching.((block(ir, v),), catches[handler])
      insertafter!(ir, v, Expr(:catch, handler, args...))
    end
  end
  for b in blocks(ir)
    current = b.id
    rename(ex) = prewalk(x -> x isa Slot ? reaching(b, x) : x, ex)
    for (v, st) in b
      ex = st.expr
      if isexpr(ex, :(=)) && ex.args[1] isa Slot
        defs[b.id][ex.args[1]] = rename(ex.args[2])
        catchbranch!(v, ex.args[1])
        delete!(ir, v)
      elseif isexpr(ex, :enter)
        catches[ex.args[1]] = slotsused(block(ir, ex.args[1]))
        push!(handlers, ex.args[1])
        catchbranch!(v)
      elseif isexpr(ex, :leave) && !haskey(catches, current)
        pop!(handlers)
      else
        ir[v] = rename(ex)
      end
    end
    for i = 1:length(BasicBlock(b).branches)
      BasicBlock(b).branches[i] = rename(BasicBlock(b).branches[i])
    end
    for (succ, ss) in todo[b.id], br in branches(b, succ)
      append!(br.args, [reaching(b, v) for v in ss])
    end
  end
  return ir
end

