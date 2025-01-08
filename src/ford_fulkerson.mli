open Graph

type paths = id list

val find_path : int graph -> int list -> id -> id -> paths option

val max_flow_path : int graph -> int -> paths -> int

val modify_flow : int graph -> int -> paths -> int graph

val inter_residuel_graph : int graph -> int graph -> string graph

val ford_fulkerson : int graph -> id -> id -> string graph
