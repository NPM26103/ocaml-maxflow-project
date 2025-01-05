(*Scenario: a company that needs to assign j jobs to w workers.
Each worker is qualified to perform certain jobs. 
The goal is to maximize the number of job assignments such that each job 
is assigned to exactly one worker, and each worker is assigned at most one job.*)

open Ford_fulkerson

type flot = { 
  capa : int;
  flot : int
}

type worker = int
type job = int

type bipartite_graph = {
  workers : worker list;
  jobs : job list;
  qualifications : (worker * job) list;
}

type matching = (worker * job) list

(* Reads the bipartite graph data from txt file *)
val read_input : string -> bipartite_graph

(* Constructs the flow network from the bipartite graph *)
val construct_flow_network : bipartite_graph -> flot graph

(* Finds the maximum matching using Ford-Fulkerson algorithm *)
val find_max_matching : flot graph -> int -> int -> worker list -> job list -> matching

(* Exports output file *)
val export_matching : matching -> string -> unit