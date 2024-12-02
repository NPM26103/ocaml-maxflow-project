open Graph

let clone_nodes (gr: 'a graph) : 'b graph = 
  n_fold gr new_node empty_graph

let gmap (gr: 'a graph) (f: 'a -> 'b) : 'b graph =
  e_fold gr (fun g src tgt lbl -> new_arc g src tgt (f lbl)) (clone_nodes gr)

let add_arc (gr: int graph) (id1: id) (id2: id) (n: int) : int graph = 
  let existing_label = find_arc gr id1 id2 in
  let new_label = match existing_label with
    | None -> n
    | Some x -> x + n
  in new_arc gr id1 id2 new_label
