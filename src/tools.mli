open Graph

val clone_nodes : 'a graph -> 'b graph
val gmap : 'a graph -> ('a -> 'b) -> 'b graph
val add_arc : id graph -> id -> id -> id -> id graph