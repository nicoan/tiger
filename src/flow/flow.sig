signature flow =
sig
  type instrTable = ((graph.node, assem.instr) table.Tabla) ref
  type tempTable = (graph.node, temp.temp list) table.Tabla
  type boolTable = (graph.node, bool) table.Tabla

  datatype flowgraph =
    FGRAPH of {
      control: graph.graph,
      def: tempTable ref,
      use: tempTable ref,
      ismove: boolTable ref
    }

  val newFlowGraph: unit -> flowgraph
  val instrs2graph: assem.instr list -> (flowgraph * instrTable)
end
