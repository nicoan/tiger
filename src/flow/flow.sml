structure flow :> flow =
struct

  open graph
  open table
  open assem

  type instrTable = ((graph.node, assem.instr) table.Tabla) ref
  type tempTable  = (graph.node, temp.temp list) table.Tabla
  type boolTable  = (graph.node, bool) table.Tabla

  datatype flowgraph =
    FGRAPH of {
      control: graph.graph,
      def: tempTable ref,
      use: tempTable ref,
      ismove: boolTable ref
    }

  fun newFlowGraph() =
      let
        val g = graph.newGraph()
        val bTable = ref (tabNueva())
        val useTable = ref (tabNueva())
        val defTable = ref (tabNueva())
        val fg = FGRAPH {control = g,
            def = defTable,
            use = useTable,
            ismove = bTable}
      in
        fg
      end

  fun instrs2graph l =
      let
        val iTable:instrTable = ref (tabNueva())
        val (FGRAPH fg) = newFlowGraph()

        fun createNodes [] labelList = labelList
          | createNodes (x::xs) labelList =
            let
              val n = newNode (#control fg)
              val _ = iTable := tabInserta(n, x, !iTable)
            in
              case x of
                LABEL {lab=l, ...} => createNodes xs ((l, n)::labelList)
              | _ => createNodes xs labelList
            end

        val labelList = createNodes l []

        fun admittedRegs r = (aux.inList r ["eax", "ebx", "ecx", "edx", "esi", "edi"]) orelse (String.isPrefix "T" r)

        fun getLabelNode [] l' = raise Fail ("Error al buscar el label " ^ l' ^ " en la lista de labels.")
          | getLabelNode ((l, n)::xs) l' = if l = l' then n
            else getLabelNode xs l'

        fun instrs2graph' [] n (FGRAPH fg) labelList = fg
          | instrs2graph' (x::xs) n (FGRAPH fg) labelList =
            let
              val (n2, lastNode) = if xs <> [] then (n + 1, false)
                else (~1, true)
              val _ =
                case x of
                  OPER {assem = s, dst = dst, src = src, jump = jmp} =>
                    let
                      val validDst = List.filter admittedRegs dst
                      val validSrc = List.filter admittedRegs src
                      val _ = if not (List.null validDst) then (#def fg) := tabInserta(n, validDst, !(#def fg))
                        else ()
                      val _ = if not (List.null validSrc) then (#use fg) := tabInserta(n, validSrc, !(#use fg))
                        else ()
                      val _ = (#ismove fg) := tabInserta(n, false, !(#ismove fg))
                      val _ = case jmp of
                          NONE => if lastNode then ()
                            else mk_edge (#control fg) {from=n, to=n2}
                        | SOME l =>
                          let
                            val labnode_l = map (getLabelNode labelList) l
                            val _ = map (fn nl => mk_edge (#control fg) {from=n, to=nl}) labnode_l
                          in
                            ()
                          end
                    in
                      ()
                    end

                | MOVE {dst = dst, src = src, ...} =>
                  let
                    val _ = if admittedRegs dst then (#def fg) := tabInserta(n, [dst], !(#def fg))
                      else ()
                    val _ = if admittedRegs src then (#use fg) := tabInserta(n, [src], !(#use fg))
                      else ()
                    val _ = (#ismove fg) := tabInserta(n, true, !(#ismove fg))
                    val _ = if lastNode then ()
                      else mk_edge (#control fg) {from=n, to=n2}
                  in
                    ()
                  end
                | _ =>
                  let
                    val _ = tabInserta(n, false, !(#ismove fg))
                    val _ = if lastNode then ()
                      else mk_edge (#control fg) {from=n, to=n2}
                  in
                    ()
                  end

            in
              instrs2graph' xs (n + 1) (FGRAPH fg) labelList
            end

        val flow = instrs2graph' l 0 (FGRAPH fg) labelList
      in
        (FGRAPH flow, iTable)
      end
end

