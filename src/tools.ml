open Graph

let clone_nodes (gr: 'a graph) : 'b graph = 
  n_fold gr (fun gr' id -> new_node gr' id) empty_graph

let gmap (gr: 'a graph) (f: 'a -> 'b) : 'b graph =
  let new_graph = clone_nodes gr in
  e_fold gr (fun g arc -> new_arc g {src=arc.src; tgt=arc.tgt; lbl = (f arc.lbl)}) new_graph

let add_arc (gr: int graph) (id1: id) (id2: id) (n: int) : int graph = 
  match find_arc gr id1 id2 with
  | Some x -> new_arc gr {src=x.src; tgt=x.tgt; lbl=x.lbl+n}
  | None -> new_arc gr {src=id1; tgt=id2; lbl=n}