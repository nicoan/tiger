structure env =
struct

	open ast
	open table
	open types

	datatype EnvEntry =
		VIntro of {access: translate.access, level: int}	(* int readonly *)
	| Var of {ty: Tipo, access: translate.access, level: int}
	| Func of {level: translate.level, label: temp.label,
		formals: Tipo list, result: Tipo, extern: bool}
end
