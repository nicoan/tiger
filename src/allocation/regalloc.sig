signature regalloc =
sig
	val simpleregalloc : frame.frame -> assem.instr list -> assem.instr list
end
