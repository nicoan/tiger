structure liveness :> liveness =
struct

  open graph
  open temp
  open table
  open flow
  open aux

  type liveSet = (graph.node, string Splayset.set) table.Tabla

  datatype igraph =
    IGRAPH of {graph: graph.graph,
      tnode: (temp.temp, graph.node) table.Tabla ref,
      gtemp: (graph.node, temp.temp) table.Tabla ref,
      moves: (graph.node * graph.node) list ref}

  fun newInterGraph() =
      let
        val g = newGraph()
        val tn = ref (tabNueva())
        val gt = ref (tabNueva())
        val m = ref []
        val ig = IGRAPH {
            graph = g,
            tnode = tn,
            gtemp = gt,
            moves = m}
      in
        ig
      end


  fun insertNodesLiv _ [] = ()
    | insertNodesLiv (IGRAPH ig) (n::ns) =
      let
        val m = newNode (#graph ig)
        val _ = (#tnode ig) := tabInserta(n, m, !(#tnode ig))
        val _ = (#gtemp ig) := tabInserta(m, n, !(#gtemp ig))
      in
        insertNodesLiv (IGRAPH ig) ns
      end

  fun printSet s = List.foldr (fn (x, xs) => x ^ "," ^ xs) "" (Splayset.listItems s)
  fun printLiveT t = map (fn (x,y) => print ( (Int.toString x) ^ " -> " ^ (printSet y) ^ "\n") ) (table.tabAList t)

  fun nodeToTemp (IGRAPH ig) x = case tabBusca(x, !(#gtemp ig)) of
        NONE => raise Fail ("No se encontro el nodo " ^ Int.toString(x) ^ " (nodToTemp)")
      | SOME n => n

  fun tempToNode (IGRAPH ig) x = case tabBusca(x, !(#tnode ig)) of
        NONE => raise Fail ("No se encontro el nodo " ^ x ^ " (tempToNode)")
      | SOME n => n

  fun liveAnalysis (FGRAPH fg) =
      let
        fun initList ns = let
              val t = ref (tabNueva():liveSet)
              val _ = List.map (fn x => t := tabInserta(x, Splayset.empty String.compare, !t)) ns
            in
              t
            end
        val ns = (nodes (#control fg))
        val liveIn = initList ns
        val liveOut = initList ns

        fun getSuccIn n = let
              val succs = succ (#control fg) n
            in
              List.map (fn x => case tabBusca(x, !liveIn) of
                    NONE => raise Fail "Error al buscar un nodo en la tabla liveIn (1)"
                  | SOME c => c) succs
            end

        fun liveAnalysis' ((FGRAPH fg), []) = (!liveIn, !liveOut)
          | liveAnalysis' ((FGRAPH fg), (n::ns)) =
            let
              val inn' = ref (Splayset.empty String.compare)
              val out' = ref (Splayset.empty String.compare)
              val inn = ref (Splayset.empty String.compare)
              val out = ref (Splayset.empty String.compare)
              val use = case tabBusca(n, !(#use fg)) of
                  NONE => Splayset.empty String.compare
                | SOME tList => aux.listToSet String.compare tList
              val def = case tabBusca(n, !(#def fg)) of
                  NONE => Splayset.empty String.compare
                | SOME tList => aux.listToSet String.compare tList

              val _ = inn := Splayset.union (use, (Splayset.difference (!out, def)));
              val _ = out := List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n)

              fun repeat() =
                  (inn' := !inn;
                    out' := !out;
                    inn := Splayset.union (use, (Splayset.difference (!out, def)));
                    out := List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n))
              val _ = repeat()
              val _ = while (not (Splayset.equal (!inn, !inn') andalso Splayset.equal (!out, !out'))) do
                  repeat()
              val _ = liveIn := tabRInserta(n, !inn, !liveIn)
              val _ = liveOut := tabRInserta(n, !out, !liveOut)
            in
              liveAnalysis' ((FGRAPH fg), ns)
            end
      in
        liveAnalysis' (FGRAPH fg, ns)
      end
end

