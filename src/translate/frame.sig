signature frame =
sig

	type frame
	type register = string
	val rv : temp.temp
	val ov : temp.temp
	val fp : temp.temp
	datatype access = InFrame of int | InReg of temp.label
	val fpPrev : int
	val fpPrevLev : int
	val newFrame : {name: temp.label} -> frame
	(* Esta funcion agrega los accces al frame alojado en el level que le pasamos *)
	val addAccFrame : access -> frame -> unit
	val name : frame -> temp.label
	val string : temp.label * string -> string
	val globl : string -> string
	val formals : frame -> access list
	val allocArg : frame -> bool -> access
	val allocLocal : frame -> bool -> access
	val sp : temp.temp
	val maxRegFrame : frame -> int
	val wSz : int
	val log2WSz : int
	val argregs : temp.temp list
	val calldefs : temp.temp list
	val callersaves : temp.temp list
	val calleesaves : temp.temp list
	val exp : access -> tree.exp
	val externalCall : string * tree.exp list -> tree.exp
	val procEntryExit1 : frame * tree.stm -> tree.stm
	val procEntryExit3 : frame * assem.instr list -> assem.instr list
	val printAccess: access -> unit
	datatype frag = PROC of {body: tree.stm, frame: frame}
	| STRING of temp.label * string

end
