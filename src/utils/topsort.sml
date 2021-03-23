local (* Sort topolo'gico *)
	fun cmp(x, y) = x=y
	fun mem(x, []) = false
		| mem(x, y::l) = cmp(x, y) orelse mem(x, l)
	fun nexts(a, []) = []
		| nexts(a, (x, y)::pairs) =
			if cmp(a, x) then y::nexts(a, pairs) else nexts(a, pairs)
in
	fun topsort graph =
			let	fun sort([], path, visited) = visited
					| sort(x::xs, path, visited) =
						if mem(x, path) then raise Fail "ciclo!"
						else sort(xs, path,
								if mem(x, visited) then visited
								else x::sort(nexts(x, graph), x::path, visited))
				val (starts, _) = ListPair.unzip graph
			in	sort(starts, [], []) end
end
