open Graph

type flot = { 
  capa : int;
  flot : int
}

val create_flot_graph : id graph -> flot graph

(* Compute residual capacity for an edge *)
val arcs_residual : flot arc -> flot graph -> flot arc * flot arc

(* Build residual graph *)
val graph_residual : flot graph -> flot graph

(* Check if an edge has positive residual capacity *)
val arc_valid : flot arc -> bool

(* Find path using BFS in residual graph *)
val find_path_gr : flot graph -> id -> id -> id list option

(* Adds a new edge or updates an existing edge in the flot graph *)
val add_flot_arc : flot graph -> id -> id -> flot -> flot graph

(* Find minimum path capacity and update flow *)
val process_path : flot graph -> id list -> flot graph * int

val export_flot_graph : flot graph -> string -> unit

val ford_fulkerson : id graph -> id -> id -> unit