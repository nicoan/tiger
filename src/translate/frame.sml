(*
LA FUNCION FORMALS TIENE QUE DEVOLVER UNA LISSTA CON LOS ACCESOS DE LOS ARGUMENTOS DE LA FUNCION
SEGUN COMO SE USEN EN LA FUNCION INCLUIDO EL STATIC LINK (ARGUMENTO 0)
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure frame :> frame = struct

	open tree
	open assem

	type level = int

	val fp = "ebp"										(* FP - frame pointer *)
	val sp = "esp"										(* SP - stack pointer *)
	val rv = "eax"										(* RV - return value  *)
	val ov = "edx"										(* OV - overflow value (edx en el 386) *)
	val wSz = 4												(* word size in bytes *)
	val log2WSz = 2										(* base two logarithm of word size in bytes *)
	val fpPrev = 0										(* offset (bytes) *)
	val fpPrevLev = 8									(* offset (bytes) *)
	val argsInicial = 3								(* words *)
	val argsOffInicial = 0						(* words *)
	val argsGap = wSz									(* bytes *)
	val regInicial = 1								(* reg *)
	val localsInicial = 0							(* words *)
	val localsGap = ~4 								(* bytes *)
	val calldefs = [rv]
	val specialregs = [rv, fp, sp]
	val argregs = []

	(* Terminology:
          o %eax, %ecx, %edx are "caller save" registers
          o %ebp, %ebx, %esi, %edi are "callee save" registers *)

	val callersaves = ["ecx", "edx"]
	val calleesaves = ["ebx", "edi", "esi"]
	type register = string
	datatype access = InFrame of int | InReg of temp.label


	fun  printAccess (InFrame k) = print ("InFrame " ^ Int.toString(k))
		|printAccess (InReg k) = print ("InReg " ^ k)

	type frame = {
			name: string,
			formals: access list ref,
			locals: bool list,
			actualArg: int ref,
			actualLocal: int ref,
			actualReg: int ref
		}

	datatype frag = PROC of {body: tree.stm, frame: frame}
	| STRING of temp.label * string

	fun newFrame{name} = {
				name=name,
				formals=ref [InFrame(fpPrevLev)],
				locals=[],
				actualArg=ref argsInicial,
				actualLocal=ref localsInicial,
				actualReg=ref regInicial
			}

	fun addAccFrame access (frame:frame) = ((#formals frame) := !(#formals frame) @ [access] ; ())

	fun name(f: frame) = #name f

	fun string(l, "") = ""
		| string(l, s) = l^":\n\t.long " ^ Int.toString(String.size(s) - 2) ^ "\n\t.string "^temp.makeString(s)^"\n\n"

	fun globl(l) = "\t.globl\t "^ l ^"\n"

	fun formals({formals=f, ...}: frame) = !f

	fun maxRegFrame(f: frame) = !(#actualReg f)

	fun allocArg (f: frame) b =
			case true of
				true =>
					let	val ret = (!(#actualArg f)*wSz+argsOffInicial)
						val _ = #actualArg f := !(#actualArg f)+1
					in	InFrame ret end
			| false => InReg(temp.newtemp())

	fun allocLocal (f: frame) b =
			case b of
				true =>
					let	val ret = InFrame(!(#actualLocal f)*4 + localsGap)
					in	#actualLocal f:=(!(#actualLocal f)-1); ret end
			| false => InReg(temp.newtemp())

	fun exp(InFrame k) = MEM(BINOP(PLUS, TEMP(fp), CONST k))
		| exp(InReg l) = TEMP l

	fun externalCall(s, l) = CALL(NAME s, l)

	fun seq [] = EXP (CONST 0)
		| seq [s] = s
		| seq (x::xs) = SEQ (x, seq xs)

	fun procEntryExit1 (frame,body) =
			let
				val temps = List.map (fn _ => temp.newtemp()) (calleesaves)
				val tempsAndRegs = ListPair.zip (temps, calleesaves)
				val moves = List.map (fn (t, r) => tree.MOVE(TEMP t, TEMP r)) tempsAndRegs
				val unMoves = List.map (fn (t, r) => tree.MOVE(TEMP r, TEMP t)) (List.rev tempsAndRegs)
			in
				seq(moves @ [body] @ unMoves)
			end

	fun procEntryExit3 (frame:frame, instrs) =
			let
				val label = [List.hd(instrs)]
				val prologo = [assem.OPER {assem="enter $" ^ Int.toString((!(#actualLocal frame)) * (~4)) ^ ",$0x0\n", src=[], dst=[], jump=NONE}]
				val epilogo = [assem.OPER {assem="leave", src=[], dst=[], jump=NONE},
						assem.OPER {assem="ret", src=[], dst=[], jump=NONE}]
			in
				label @ prologo @ (List.tl(instrs)) @ epilogo
			end
end
