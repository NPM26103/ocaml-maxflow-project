open Graph

type paths = id list

(* Find path using BFS *)
val find_path : int graph -> id -> id -> paths option

(* Find the minimum capacity along the path *)
val max_flow_path : int graph -> int -> paths -> int

(* Update flow and residual graph *)
val modify_flow : int graph -> int -> paths -> int graph

(* Create residual graph *)
val inter_residuel_graph : int graph -> int graph -> string graph

(* Ford-Fulkerson Algorithm *)
val ford_fulkerson : int graph -> id -> id -> string graph
