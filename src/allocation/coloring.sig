signature coloring =
sig
    type adjListT    = (graph.node Splayset.set) array
    type wListMoves  = graph.node Splayset.set ref
    type adjSetT     = graph.edge Splayset.set ref

    val coloring : ((assem.instr list) * frame.frame * bool) -> ((assem.instr list) * frame.frame * ((temp.temp, string) table.Tabla))
    val replaceTforColors : (assem.instr list) -> ((temp.temp, string) table.Tabla) -> (assem.instr list)
end
