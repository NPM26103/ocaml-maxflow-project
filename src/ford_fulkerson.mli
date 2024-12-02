open Graph

val init : int graph -> int graph
val find_path : int graph -> id -> id -> (id list option)
val increase_flow : int graph -> (id list) -> int -> int graph
val ford_fulkerson : int graph -> id -> id -> int graph