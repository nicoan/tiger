signature liveness =
sig
  type liveSet = (graph.node, string Splayset.set) table.Tabla

  datatype igraph =
    IGRAPH of {
      graph: graph.graph,
      tnode: (temp.temp,graph.node) table.Tabla ref,
      gtemp: (graph.node, temp.temp) table.Tabla ref,
      moves: (graph.node * graph.node) list ref
    }

  val newInterGraph: unit -> igraph
  val liveAnalysis: flow.flowgraph -> liveSet * liveSet

  val nodeToTemp: igraph -> int -> string
  val tempToNode: igraph -> string -> int

  val insertNodesLiv: igraph -> temp.temp list -> unit

end

