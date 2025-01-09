open Graph

val create_nodes : int list -> 'a graph

val connect_all_nodes : int graph -> int list -> int graph

val create_final_graph : int graph -> (int * int) list -> int graph

val extract_settlements : string graph -> (int * int * int) list

val solve : int list -> (int * int) list -> (int * int * int) list