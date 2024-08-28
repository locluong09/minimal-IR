module IRTools

export @code_ir

module Inner

  using MacroTools
  using MacroTools: @q, prewalk, postwalk
  import ..IRTools

  export @code_ir

  @nospecialize

 
  include("ir-m.jl")
  include("reflection.jl")
  include("utils.jl")
  include("wrapper.jl")


end

let exports = :[
      IRTools, IR, Block, BasicBlock, Variable, Statement, Branch, Slot, branch, var, stmt, arguments, argtypes,
      branches, undef, unreachable, isreturn, isconditional, block!, deleteblock!, branch!, argument!, return!,
      canbranch, returnvalue, returntype, emptyargs!, deletearg!, block, blocks, successors, predecessors,
      xcall, exprtype, exprline, isexpr, insertafter!, explicitbranch!, prewalk, postwalk,
      prewalk!, postwalk!, finish, substitute!, substitute, ssa!, prune!, renumber, merge_returns!, expand!,
      ].args
  append!(exports, Symbol.(["@code_ir"]))
  for x in exports
    @eval const $x = Inner.$x
  end
  @eval module All
    $([:(import ..IRTools: $x) for x in exports]...)
    export $(exports...)
  end
end

end 
